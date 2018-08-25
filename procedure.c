#include <stdlib.h>
#include <stdio.h>

#include <gc.h>

#include "io.h"
#include "symbol.h"
#include "error.h"
#include "procedure.h"
#include "primitive_procedure.h"

type*  
mk_procedure(type* parameters, type* body, type* env)
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_PROCEDURE: could not allocate memory for type");
        exit(1);
    }
    
    result->type = PROCEDURE;
    result->data = cons(parameters, cons(body, cons(env, nil())));

    return result;
}

type*
is_compound_procedure(const type* procedure)
{
    return mk_boolean(!is_nil(procedure) && procedure->type == PROCEDURE);
}

type* 
procedure_parameters(type* procedure)
{
    if (!is_true(is_compound_procedure(procedure))) 
    {
        display_debug(procedure);
        throw_error(TYPE_ERROR,
                    "PROCEDURE_PARAMETERS: procedure must be a procedure");
    }

    return car(procedure->data);
}

type* 
procedure_body(type* procedure)
{
    assert_throw(is_true(is_compound_procedure(procedure)),
                 TYPE_ERROR,
                 "PROCEDURE_BODY: procedure must be a procedure");

    return car(cdr(procedure->data));
}

type* 
procedure_environment(type* procedure)
{
    assert_throw(is_true(is_compound_procedure(procedure)),
                 TYPE_ERROR,
                 "PROCEDURE_ENVIRONMENT: procedure must be a procedure");

    return car(cdr(cdr(procedure->data)));
}

