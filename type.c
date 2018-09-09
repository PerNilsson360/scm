#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <assert.h>

#include <gc.h>

#include "io.h"
#include "symbol.h"
#include "number.h"
#include "common.h"
#include "error.h"
#include "type.h"
#include "eval.h"
#include "util.h"

type* 
mk_bound_var(type* symbol, 
             unsigned int frame_index, 
             unsigned int var_index,
             int is_inproper_list)
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_BOUND_VAR: could not allocate memory for type");
        exit(1);
    }

    result->type = BOUND_VAR;
    result->data = mloc(sizeof(BOUND_VAR_DATA));
    
    if (result->data == NULL)
    {
        fprintf(stderr, "MK_VAR_VAR: could not allocate memory for data");
        exit(1);
    }

    ((BOUND_VAR_DATA*) result->data)->symbol = symbol;
    ((BOUND_VAR_DATA*) result->data)->frame_index = frame_index;
    ((BOUND_VAR_DATA*) result->data)->var_index = var_index;
    ((BOUND_VAR_DATA*) result->data)->is_inproper_list = is_inproper_list;

    return result;
}

int is_bound_var(const type* exp)
{
    return exp->type == BOUND_VAR;
}

type* 
mk_none()
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_NONE: could not allocate memory for type");
        exit(1);
    }

    result->type = NONE;
    result->data = 0;

    return result;
}

int 
is_none(const type* sexp)
{
    return sexp->type == NONE;
}


type* 
cons(const type* car, const type* cdr)
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "CONS: could not allocate memory for type");
        exit(1);
    }

    result->type = PAIR;
    result->data = mloc(sizeof(PAIR_DATA));

    if (result->data == NULL)
    {
        fprintf(stderr, "CONS: could not allocate memory for data");
        exit(1);
    }

    ((PAIR_DATA*) result->data)->car = (type*) car;
    ((PAIR_DATA*) result->data)->cdr = (type*) cdr;

    return result; 
}

type* 
car(const type* list)
{
    if (list->type != PAIR)
    {
        display_debug(list);
        printf("not a pair\n");
        /*assert_throw(FALSE, TYPE_ERROR, "CAR: not a pair");*/
	assert(0);  
    }

    return ((PAIR_DATA*) list->data)->car;
}

type* 
cdr(const type* list)
{
    if (list->type != PAIR)
    {
        display_debug(list);
        printf("not a pair\n");
        assert_throw(FALSE, TYPE_ERROR, "CDR: not a pair");
    }

    return ((PAIR_DATA*) list->data)->cdr;
}

void 
set_car(type* list, const type* value)
{
    assert_throw(is_pair(list), TYPE_ERROR, "SET-CAR: not a pair");

    ((PAIR_DATA*) list->data)->car = (type*)value;
}

void 
set_cdr(type* list, const type* value)
{
    assert_throw(is_pair(list), TYPE_ERROR, "SET-CDR: not a pair");
    
    ((PAIR_DATA*) list->data)->cdr = (type*)value;
}

type* 
is_list(const type* sexp)
{
    type* result = nil();

    if (is_nil(sexp))
    {
        result = mk_boolean(TRUE);
    }
    else if (!is_pair(sexp))
    {
        result = mk_boolean(FALSE);
    }
    else
    {
        result = is_list(cdr(sexp));
    }
    
    return result;
}

int 
is_pair(const type* pair)
{
    return pair->type == PAIR;
}

int 
is_empty_pair(const type* sexp)
{
    return is_pair(sexp) && is_nil(car(sexp)) && is_nil(cdr(sexp));
}

unsigned int 
length(const type* pair)
{
    unsigned int result = 0;
        
    assert(is_pair(pair) || is_nil(pair));

    if (!is_nil(pair))
    {
        result = 1 + length(cdr(pair));
    }
    
    return result;
}

int
is_eq(const type* left, const type* right)
{
    return left->data == right->data;
}

int
is_eqv(const type* left, const type* right)
{
    int result = FALSE;
    if (is_number(left) && is_number(right))
    {
        result = is_number_equal(left, right);
    }
    else
    {
        result = left->data == right->data;
    }
    
    return result;
}

type* 
mk_quoted(const type* sexp)
{
    return cons(_quote_keyword_symbol_, cons(sexp, nil()));
}

int 
is_quoted(const type* sexp)
{
    return is_tagged_list(sexp, _quote_keyword_symbol_) && length(sexp) < 3;
}

type* 
quotation_value(const type* sexp)
{
    return car(cdr(sexp));
}

type* 
mk_boolean(int t)
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_BOOLEAN: could not allocate memory for type");
        exit(1);
    }

    result->type = BOOLEAN;
    
    result->data = (void*) t;
    
    return result;
}

int 
is_boolean(const type* sexp)
{
    return sexp->type == BOOLEAN;
}

type* 
not(const type* sexp)
{
    return mk_boolean(!is_true(sexp));
}

int 
is_true(const type* sexp)
{
    return !(sexp != NULL && 
             sexp->type == BOOLEAN && 
             ((int) sexp->data == FALSE));
}
