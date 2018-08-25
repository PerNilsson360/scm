#include <stdlib.h>
#include <stdio.h>

#include <gc.h>

#include "type.h"
#include "error.h"
#include "number.h"
#include "vector.h"

int
is_vector(const type* sexp)
{
    return sexp->type == VECTOR;
}

type*
_mk_vector(const type* length)
{
    
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_VECTOR: could not allocate memory for type");
        exit(1);
    }

    result->type = VECTOR;
    result->data = mloc(sizeof(vector));

    if (result->data == NULL)
    {
        fprintf(stderr, "MK_VECTOR: could not allocate memory for data");
        exit(1);
    }

    ((vector*) result->data)->slots = mloc(sizeof(type*) * 
					   (unsigned int)length->data); 

    if (((vector*) result->data)->slots == NULL)
    {
        fprintf(stderr, "MK_VECTOR: could not allocate memory for slots");
        exit(1);
    }

    ((vector*) result->data)->length = (type*) length;

    return result;
}

type* 
mk_vector(const type* length, const type* obj)
{
    type* result;
    int i;

    assert_throw(is_number(length),
                 TYPE_ERROR,
                 "MK_VECTOR: length must be an integer");
    
    result = _mk_vector(length);
   
    for (i = 0; i < (int) length->data; i++)
    {
        ((vector*) result->data)->slots[i] = (type*) obj;
    }

    return result;
}

type* 
list_to_vector(const type* list)
{
    int i;
    int len = length(list);
    type* result = _mk_vector(mk_number_from_int(len));
    
    const type* hd = list;
    for (i = 0; i < len; i++) 
    {
        ((vector*) result->data)->slots[i] = (type *) car(hd);
        hd = cdr(hd);
    }

    return result;
}

type* 
vector_length(const type* sexp)
{
    assert_throw(is_vector(sexp),
                 TYPE_ERROR,
                 "VECTOR_LENGTH: wrong type in argument");

    return ((vector*) sexp->data)->length;
}


type* 
vector_ref(const type* sexp, int k)
{
    assert_throw(is_vector(sexp),
                 TYPE_ERROR,
                 "VECTOR_REF: first argument must be a vector");
    
    assert_throw(k >= 0 && k < (int) ((vector*) sexp->data)->length->data,
                 TYPE_ERROR,
                 "VECTOR_REF: k is out of range");

    return ((vector*) sexp->data)->slots[k];
}

void
vector_set(type* sexp, int k, const type* obj)
{
    assert_throw(is_vector(sexp),
                 TYPE_ERROR,
                 "VECTOR_SET: first argument must be a vector");

    assert_throw(k >= 0 && k < (int) ((vector*) sexp->data)->length->data,
                 TYPE_ERROR,
                 "VECTOR_SET: k is out of range");

    ((vector*) sexp->data)->slots[k] = (type*) obj;
}

