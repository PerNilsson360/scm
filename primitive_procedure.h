#ifndef _PRIMITIVE_PROCEDURE_HH_
#define _PRIMITIVE_PROCEDURE_HH_

#include "type.h"

void init_primitive_procedures();
int is_primitive_procedure(const type* procedure);
int is_symbol_primitive_procedure(const type* procedure);
type* find_primitive_procedure(const type* symbol);
type* apply_primitive_procedure(const type* procedure, 
                                const type* arguments,
                                const type* env);

#endif
