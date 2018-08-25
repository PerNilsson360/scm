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

type* 
mk_env(type* data)
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_ENV: could not allocate memory for type");
        exit(1);
    }

    result->type = ENVIRONMENT;
    result->data = data;

    return result; 
}

int
is_env(const type* sexp)
{
    return sexp->type == ENVIRONMENT;
}

int
is_empty_env(const type* env)
{
    assert(is_env(env));
    return is_nil(env->data);
}

type*
enclosing_environment(const type* env)
{
    return cdr(env->data);
}

int
is_last_frame(const type* env)
{
    return is_empty_env(enclosing_environment(env));
}

type*
first_frame(type* env)
{
    return car(env->data);
}

type* 
make_frame(type* vars, type * vals)
{
    return cons(vars, vals);
}

type*
frame_vars(type* frame)
{
    return car(frame);
}

type*
frame_vals(type* frame)
{
    return cdr(frame);
}

void
add_binding_to_frame(type* var, type* val, type* frame)
{
    set_car(frame, cons(var, car(frame)));
    set_cdr(frame, cons(val, cdr(frame)));
}

type* lookup_env_loop(type* var, type* env);

int
lookup_scan(type* var, type* vars, type* vals, type** result)
{
    int rc = FALSE;

    while(!is_nil(vars))
    {
        if (is_eq(var, car(vars)))
        {
            *result = car(vals);
            rc = TRUE;
            break;
        }

        vars = cdr(vars);
        vals = cdr(vals);
    }

    return rc;
}

type* 
lookup_env_loop(type* var, type* env)
{
    type* result;
    int rc;

    while (!is_last_frame(env)) 
    {
        env = enclosing_environment(env);
    }

    type* frame = first_frame(env);    
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
type*
get_var_from_frame(unsigned int var_index, int is_inproper_list, type* vals)
{
    type* result;
    if (var_index == 0)
    {
        if (is_inproper_list) 
        {
            result = vals;
        }
        else
        {
            result = car(vals);
        }
    }
    else
    {
        result = get_var_from_frame(--var_index, is_inproper_list, cdr(vals));
    }

    return result;
}

static 
type*
get_var(unsigned int frame_index, 
        unsigned int var_index, 
        int is_inproper_list,
        type* env)
{
    type* result;

    if (frame_index == 0) 
    {
        type* f = first_frame(env);
        result = get_var_from_frame(var_index, 
                                    is_inproper_list, 
                                    frame_vals(f));
    }
    else
    {
        result = get_var(--frame_index, 
                         var_index, 
                         is_inproper_list,
                         enclosing_environment(env));
    }
    
    return result;
}

type* 
lookup_variable_value(type* var, type* env)
{
    assert(is_env(env) && "Env must be an environment");

    if (is_symbol(var))
    {
        return lookup_env_loop(var, env);
    }
    else
    {
        return get_var(((BOUND_VAR_DATA*) var->data)->frame_index,
                       ((BOUND_VAR_DATA*) var->data)->var_index,
                       ((BOUND_VAR_DATA*) var->data)->is_inproper_list,
                       env);
    }
}

type* set_env_loop(type* var, type* val, type* env);

type*
set_scan(type* var, type* val, type* vars, type* vals, type* env)
{
    type* result = nil();

    if (is_symbol(vars) || is_nil(vars))
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

type* 
set_env_loop(type* var, type* val, type* env)
{
    type* result = nil();

    if(!is_empty_env(env))
    {
        type* frame = first_frame(env);

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
        type* val,
        type* env)
{
    while (frame_index > 0)
    {
        env = enclosing_environment(env);
        frame_index--;
    }

    type* vals = frame_vals(first_frame(env));

    while (var_index > 0)
    {
        vals = cdr(vals);
        var_index--;
    }

    set_car(vals, val);
}

void 
set_variable_value(type* var, type* val, type* env)
{
    assert(is_env(env) && "Env must be a environment");

    if (is_symbol(var))
    {
        set_env_loop(var, val, env);
    }
    else
    {
        set_var(((BOUND_VAR_DATA*) var->data)->frame_index,
                ((BOUND_VAR_DATA*) var->data)->var_index,
                val,
                env);
    }
}

type* 
extend_environment(type* vars, type* vals, type* env)
{
    assert(is_env(env) && "Env must be a anvironment");
    
    type* e = mk_env(cons(make_frame(vars, vals), env));
    return e;
}

void
define_scan(type* var, type* val, type* vars, type* vals, type* frame)
{
    if (is_symbol(vars) || is_nil(vars))
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
define_variable(type* var, type* val, type* env)
{
    type* frame;

    assert(is_env(env) && "Env must be a list");

    frame = first_frame(env);
    define_scan(var, 
                val, 
                frame_vars(frame), 
                frame_vals(frame), 
                frame);
}
