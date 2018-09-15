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
    result->d.t = cons(parameters, cons(body, cons(env, nil())));

    return result;
}

TYPE*
is_compound_procedure(const TYPE* procedure)
{
    return mk_boolean(!is_nil(procedure) && procedure->type == PROCEDURE);
}

TYPE* 
procedure_parameters(TYPE* procedure)
{
    if (!is_true(is_compound_procedure(procedure))) 
    {
        display_debug(procedure);
        throw_error(TYPE_ERROR,
                    "PROCEDURE_PARAMETERS: procedure must be a procedure");
    }

    return car(procedure->d.t);
}

TYPE* 
procedure_body(TYPE* procedure)
{
    assert_throw(is_true(is_compound_procedure(procedure)),
                 TYPE_ERROR,
                 "PROCEDURE_BODY: procedure must be a procedure");

    return car(cdr(procedure->d.t));
}

TYPE* 
procedure_environment(TYPE* procedure)
{
    assert_throw(is_true(is_compound_procedure(procedure)),
                 TYPE_ERROR,
                 "PROCEDURE_ENVIRONMENT: procedure must be a procedure");

    return car(cdr(cdr(procedure->d.t)));
}

