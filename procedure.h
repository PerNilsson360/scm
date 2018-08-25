#ifndef _PROCEDURE_H_
#define _PROCEDURE_H_

#include "type.h"

type* mk_procedure(type* parameters, type* body, type* env);
type* is_compound_procedure(const type* procedure);
type* procedure_parameters(type* procedure);
type* procedure_body(type* procedure);
type* procedure_environment(type* procedure);

#endif
