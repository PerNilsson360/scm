// MIT license
//
// Copyright 2025 Per Nilsson
///
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
mk_env(TYPE* vars, TYPE* vals, TYPE* previous_frame)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_EMPTY_ENV: could not allocate memory for type");
        exit(1);
    }

    result->type = ENVIRONMENT;
    result->d.en = mloc(sizeof(ENVIRONMENT_DATA));
        
    if (result->d.en == NULL)
    {
        fprintf(stderr, "MK_EMPTY_ENV: could not allocate memory for data");
        exit(1);
    }
        
    result->d.en->vars = vars;
    result->d.en->vals = vals;
    result->d.en->previous_frame = previous_frame;
    return result;
}

static TYPE* global_env = NULL;

TYPE*
get_global_env()
{
    if (global_env == NULL)
    {
	global_env = mk_env(nil(), nil(), nil());
    }
        
    return global_env;
}

int
lookup_unbound_var(TYPE* var, TYPE** val)
{
    return global_env_lookup_var(var, val);
}

int
is_env(const TYPE* sexp)
{
    return sexp->type == ENVIRONMENT;
}

void
add_binding_to_frame(TYPE* var, TYPE* val, TYPE* frame)
{
    frame->d.en->vars = cons(var, frame->d.en->vars);
    frame->d.en->vals = cons(val, frame->d.en->vals);
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
        env = env->d.en->previous_frame;
    }

    result = get_var_from_frame(var_index,
				is_inproper_list,
				env->d.en->vals);

    return result;
}

TYPE* 
lookup_variable_value(TYPE* var, TYPE* env)
{
    return get_var(var->d.b->frame_index,
		   var->d.b->var_index,
		   var->d.b->is_inproper_list,
		   env);
}

void
set_var(unsigned int frame_index,
        unsigned int var_index,
        TYPE* val,
        TYPE* env)
{
    while (frame_index > 0)
    {
        env = env->d.en->previous_frame;
        frame_index--;
    }

    TYPE* vals = env->d.en->vals;

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
    if (IS_SYMBOL(var))
    {
        global_env_define_variable(var, val);
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
    return mk_env(vars, vals, env);
}

void
define_scan(TYPE* var, TYPE* val, TYPE* vars, TYPE* vals, TYPE* frame)
{
    if (IS_SYMBOL(vars) || IS_NIL(vars))
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
    if (env == global_env) {
        global_env_define_variable(var, val);
        return;
    }
    define_scan(var, val, env->d.en->vars, env->d.en->vals, env);
}
