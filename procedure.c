#include <stdlib.h>
#include <stdio.h>

#include <gc.h>

#include "io.h"
#include "util.h"
#include "symbol.h"
#include "error.h"
#include "procedure.h"
#include "primitive_procedure.h"

TYPE*  
mk_procedure(TYPE* parameters, TYPE* body, TYPE* env)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_PROCEDURE: could not allocate memory for type");
        exit(1);
    }
    
    result->type = PROCEDURE;
	result->d.pr = mloc(sizeof(PROCEDURE_DATA));

    if (result->d.pr == NULL)
    {
        fprintf(stderr, "MK_PROCEDURE: could not allocate memory for data");
        exit(1);
    }
	
	result->d.pr->parameters = parameters;
	result->d.pr->body = body;
	result->d.pr->env = env;

    return result;
}

int
is_compound_procedure(const TYPE* procedure)
{
    return procedure->type == PROCEDURE;
}

TYPE* 
procedure_parameters(TYPE* procedure)
{
    /* @todo clean upp error handling */
    if (!is_compound_procedure(procedure)) 
    {
        display_debug(procedure);
        throw_error(TYPE_ERROR,
                    "PROCEDURE_PARAMETERS: procedure must be a procedure");
    }
	
    return procedure->d.pr->parameters;
}

TYPE* 
procedure_body(TYPE* procedure)
{
    assert_throw(is_compound_procedure(procedure),
                 TYPE_ERROR,
                 "PROCEDURE_BODY: procedure must be a procedure");

    return procedure->d.pr->body;
}

TYPE* 
procedure_environment(TYPE* procedure)
{
    assert_throw(is_compound_procedure(procedure),
                 TYPE_ERROR,
                 "PROCEDURE_ENVIRONMENT: procedure must be a procedure");

    return procedure->d.pr->env;
}

