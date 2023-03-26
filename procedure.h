#ifndef _PROCEDURE_H_
#define _PROCEDURE_H_

#include "type.h"

TYPE* mk_procedure(TYPE* parameters, TYPE* body, TYPE* env);
int is_compound_procedure(const TYPE* procedure);
TYPE* procedure_parameters(TYPE* procedure);
TYPE* procedure_body(TYPE* procedure);
TYPE* procedure_environment(TYPE* procedure);

#endif
