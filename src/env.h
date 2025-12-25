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
#ifndef _ENV_H_
#define _ENV_H_
#include "type.h"
/**
 * Returns the global environment.
 */
TYPE* get_global_env();

/**
 * Given an unbound variable (i.e. symbol) find its value
 * in the global environment.
 */
TYPE* lookup_unbound_var(TYPE* var);

int is_env(const TYPE* sexp);

/**
 * Looks up var in the environment env.
 *
 * @requires var is not null and is a symbol.
 * @requires env is a list
 * 
 * @ensures lookup_variable_value(var, define_variable(var, val, _)) == val &
 *          lookup_variable_value(var, empty_env) throws exception
 */
TYPE* lookup_variable_value(TYPE* var, TYPE* env);


/**
 * Adds the var, value association to a new frame in the environment env.
 * 
 * @requires vars, vals and env are lists. vars is a list of symbols.
 * 
 * @ensures let env = extend_environment(vars, vals, _)
 forall var, val in vars, vals. 
 *                           lookup_variable_value(var, env) = val
 */
TYPE* extend_environment(TYPE* vars, TYPE* vals, TYPE* env);

/**
 * Adds the binding var, val to the environment var has to be previosly defined
 *
 * @requires var is a symbol, env is a list
 *
 * @ensure lookup_variable_value(var, 
 *               set_variable(var, val, define_variable(var, _, env))) == val &
 *         lookup_variable_value(var, 
 *                               set_variable_value(var, 
 *                                                  val, 
 *                                                  empty_env)) throws exception
 */
void set_variable_value(TYPE* var, TYPE* val, TYPE* env);


/**
 * Adds the binding var, val to the environment
 *
 * @requires var is a symbol, env is a list
 *
 * @ensure lookup_variable_value(var, define_variable(var, val, env)) == val
 */
void define_variable(TYPE* var, TYPE* val, TYPE* env);

#endif
