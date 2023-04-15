#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stddef.h>

#include <gc.h>

#include "io.h"
#include "type.h"
#include "symbol.h"
#include "error.h"
#include "env.h"
#include "util.h"
#include "common.h"
#include "primitive_procedure.h"

TYPE* 
mk_env(TYPE* data)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_ENV: could not allocate memory for type");
        exit(1);
    }

    result->type = ENVIRONMENT;
    result->d.t = data;

    return result; 
}

int
is_env(const TYPE* sexp)
{
    return sexp->type == ENVIRONMENT;
}

int
is_empty_env(const TYPE* env)
{
    return IS_NIL(env->d.t);
}

TYPE*
enclosing_environment(const TYPE* env)
{
    return cdr(env->d.t);
}

int
is_last_frame(const TYPE* env)
{
    return is_empty_env(enclosing_environment(env));
}

TYPE*
first_frame(TYPE* env)
{
    return car(env->d.t);
}

TYPE* 
make_frame(TYPE* vars, TYPE* vals)
{
    return cons(vars, vals);
}

TYPE*
frame_vars(TYPE* frame)
{
    return car(frame);
}

TYPE*
frame_vals(TYPE* frame)
{
    return cdr(frame);
}

void
add_binding_to_frame(TYPE* var, TYPE* val, TYPE* frame)
{
    set_car(frame, cons(var, car(frame)));
    set_cdr(frame, cons(val, cdr(frame)));
}

TYPE* lookup_env_loop(TYPE* var, TYPE* env);

int
lookup_scan(TYPE* var, TYPE* vars, TYPE* vals, TYPE** result)
{
    int rc = FALSE;

    while (!IS_NIL(vars))
    {
        if (is_eq(var, vars->d.p->car))
        {
            *result = vals->d.p->car;
            rc = TRUE;
            break;
        }

        vars = vars->d.p->cdr;
        vals = vals->d.p->cdr;
    }

    return rc;
}

TYPE* 
lookup_env_loop(TYPE* var, TYPE* env)
{
    TYPE* result;
    int rc;

    while (!is_last_frame(env)) 
    {
        env = enclosing_environment(env);
    }

    TYPE* frame = first_frame(env);    
    rc = lookup_scan(var, frame_vars(frame), frame_vals(frame), &result);

    if(rc == FALSE)
    {
        if (is_symbol_primitive_procedure(var))
        {
            result = find_primitive_procedure(var);
        }
        else
        {
            printf("Missing var: ");
            display(var);
            throw_error(EVAL_ERROR, "\nLOOKUP_ENV_LOOP: could not find var");
        }
    }
    
    return result;
}

static
TYPE*
get_var_from_frame(unsigned int var_index, int is_inproper_list, TYPE* vals)
{
    for (;var_index != 0; var_index--)
    {
		vals = vals->d.p->cdr;
    }
    
    return is_inproper_list ? vals : vals->d.p->car;
}

static 
TYPE*
get_var(unsigned int frame_index, 
        unsigned int var_index, 
        int is_inproper_list,
        TYPE* env)
{
    TYPE* result;

    for (;frame_index != 0; frame_index--)
    {
    	env = env->d.t->d.p->cdr;
    }

    TYPE* f = first_frame(env);
    result = get_var_from_frame(var_index,
    				is_inproper_list,
    				frame_vals(f));
    
    return result;
}

TYPE* 
lookup_variable_value(TYPE* var, TYPE* env)
{
    assert(is_env(env) && "Env must be an environment");

    if (is_symbol(var))
    {
        return lookup_env_loop(var, env);
    }
    else
    {
        return get_var(var->d.b->frame_index,
                       var->d.b->var_index,
                       var->d.b->is_inproper_list,
                       env);
    }
}

TYPE* set_env_loop(TYPE* var, TYPE* val, TYPE* env);

TYPE*
set_scan(TYPE* var, TYPE* val, TYPE* vars, TYPE* vals, TYPE* env)
{
    TYPE* result = nil();

    if (is_symbol(vars) || IS_NIL(vars))
    {
        result = set_env_loop(var, val, enclosing_environment(env));
    }
    else if (is_eq(var, car(vars)))
    {
        set_car(vals, val);
    }
    else
    {
        result = set_scan(var, val, cdr(vars), cdr(vals), env);
    }

    return result;
}

TYPE* 
set_env_loop(TYPE* var, TYPE* val, TYPE* env)
{
    TYPE* result = nil();

    if(!is_empty_env(env))
    {
        TYPE* frame = first_frame(env);

        result = set_scan(var, val, frame_vars(frame), frame_vals(frame), env);
    }
    else
    {
        display(var);
        throw_error(EVAL_ERROR, "\nSET_ENV_LOOP: could not find var");
    }
    
    return result;
}

void
set_var(unsigned int frame_index,
        unsigned int var_index,
        TYPE* val,
        TYPE* env)
{
    while (frame_index > 0)
    {
        env = enclosing_environment(env);
        frame_index--;
    }

    TYPE* vals = frame_vals(first_frame(env));

    while (var_index > 0)
    {
        vals = cdr(vals);
        var_index--;
    }

    set_car(vals, val);
}

void 
set_variable_value(TYPE* var, TYPE* val, TYPE* env)
{
    assert(is_env(env) && "Env must be a environment");

    if (is_symbol(var))
    {
        set_env_loop(var, val, env);
    }
    else
    {
        set_var(var->d.b->frame_index,
                var->d.b->var_index,
                val,
                env);
    }
}

TYPE* 
extend_environment(TYPE* vars, TYPE* vals, TYPE* env)
{
    assert(is_env(env) && "Env must be a anvironment");
    
    TYPE* e = mk_env(cons(make_frame(vars, vals), env));
    return e;
}

void
define_scan(TYPE* var, TYPE* val, TYPE* vars, TYPE* vals, TYPE* frame)
{
    if (is_symbol(vars) || IS_NIL(vars))
    {
        add_binding_to_frame(var, val, frame);
    }
    else if (is_eq(var, car(vars)))
    {
        set_car(vals, val);
    }
    else
    {
        define_scan(var, val, cdr(vars), cdr(vals), frame);
    }
}

void 
define_variable(TYPE* var, TYPE* val, TYPE* env)
{
    TYPE* frame;

    assert(is_env(env) && "Env must be a list");

    frame = first_frame(env);
    define_scan(var, 
                val, 
                frame_vars(frame), 
                frame_vals(frame), 
                frame);
}
