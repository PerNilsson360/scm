#ifndef _PRIMITIVE_PROCEDURE_HH_
#define _PRIMITIVE_PROCEDURE_HH_

#include "type.h"

void init_primitive_procedures();
int is_primitive_procedure(const TYPE* procedure);
int is_symbol_primitive_procedure(const TYPE* procedure);
TYPE* find_primitive_procedure(const TYPE* symbol);
TYPE* apply_primitive_procedure(const TYPE* procedure, 
                                const TYPE* arguments,
                                const TYPE* env);

#endif
