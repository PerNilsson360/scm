#ifndef _ENV_H_
#define _ENV_H_
#include "type.h"

type* mk_env(type* data);
type* mk_first_env_frame();

int is_env(const type* sexp);

/**
 * Looks up var in the environment env.
 *
 * @requires var is not null and is a symbol.
 * @requires env is a list
 * 
 * @ensures lookup_variable_value(var, define_variable(var, val, _)) == val &
 *          lookup_variable_value(var, empty_env) throws exception
 */
type* lookup_variable_value(type* var, type* env);


/**
 * Adds the var, value association to a new frame in the environment env.
 * 
 * @requires vars, vals and env are lists. vars is a list of symbols.
 * 
 * @ensures let env = extend_environment(vars, vals, _)
                      forall var, val in vars, vals. 
 *                           lookup_variable_value(var, env) = val
 */
type* extend_environment(type* vars, type* vals, type* env);

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
void set_variable_value(type* var, type* val, type* env);


 /**
 * Adds the binding va, val to the environment
 *
 * @requires var is a symbol, env is a list
 *
 * @ensure lookup_variable_value(var, define_variable(var, val, env)) == val
 */
void define_variable(type* var, type* val, type* env);

#endif
