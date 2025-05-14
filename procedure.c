#include <stdlib.h>
#include <stdio.h>

#include <gc.h>

#include "io.h"
#include "util.h"
#include "common.h"
#include "symbol.h"
#include "error.h"
#include "procedure.h"
#include "primitive_procedure.h"

static int
is_improper_list(const TYPE* exp)
{
    if (IS_NIL(exp))
    {
        return FALSE;
    } 
    else if (is_pair(exp))
    {
        return is_improper_list(cdr(exp));
    } 
    else 
    {
        return TRUE;
    } 
}

static int
improper_list_length(const TYPE* exp)
{
    if (is_pair(exp)) 
    {
        return 1 + improper_list_length(cdr(exp));
    } 
    else
    {
        return 0;
    }
}

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
    int is_var_args =  is_improper_list(parameters);
    result->d.pr->is_var_args = is_var_args;
    if (is_var_args)
    {
        result->d.pr->param_len = improper_list_length(parameters);
    }
    else
    {
        result->d.pr->param_len = length(parameters);
    }
	result->d.pr->body = body;
	result->d.pr->env = env;

    return result;
}

int
is_compound_procedure(const TYPE* procedure)
{
    return procedure->type == PROCEDURE;
}

int
is_var_arg(const TYPE* procedure)
{
    return procedure->d.pr->is_var_args;
}

int
param_len(const TYPE* procedure)
{
    return procedure->d.pr->param_len;
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

