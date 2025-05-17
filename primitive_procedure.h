#ifndef _PRIMITIVE_PROCEDURE_HH_
#define _PRIMITIVE_PROCEDURE_HH_

#include "type.h"

void init_primitive_procedures();
void global_env_define_variable(TYPE* var, TYPE* val);
int is_primitive_procedure(const TYPE* procedure);
int is_symbol_primitive_procedure(const TYPE* procedure);
int global_env_lookup_var(const TYPE* symbol, TYPE** value);
TYPE* apply_primitive_procedure(const TYPE* procedure, 
                                const TYPE* arguments,
                                const TYPE* env);

#endif
