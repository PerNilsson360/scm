#include <stdlib.h>
#include <stdio.h>

#include <gc.h>

#include "type.h"
#include "error.h"
#include "number.h"
#include "vector.h"
#include "util.h"
#include "common.h"

int
is_vector(const TYPE* sexp)
{
    return IS_POINTER_TO_STRUCT_OF_TYPE(sexp, VECTOR);
}

TYPE*
_mk_vector(const TYPE* length)
{
    
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_VECTOR: could not allocate memory for type");
        exit(1);
    }

    result->type = VECTOR;
    result->d.v = mloc(sizeof(VECTOR_DATA));

    if (result->d.v == NULL)
    {
        fprintf(stderr, "MK_VECTOR: could not allocate memory for data");
        exit(1);
    }

    /* TODO: here we could assert and maybe make a more efficient accessor function */
    int len = as_integer(length);
    
    result->d.v->slots = mloc(sizeof(TYPE*) * len); 

    if (result->d.v->slots == NULL)
    {
        fprintf(stderr, "MK_VECTOR: could not allocate memory for slots");
        exit(1);
    }

    result->d.v->length = (TYPE*) length;

    return result;
}

TYPE* 
mk_vector(const TYPE* length, const TYPE* obj)
{
    TYPE* result;
    int i;

    assert_throw(is_integer(length),
                 TYPE_ERROR,
                 "MK_VECTOR: length must be an integer");
    
    result = _mk_vector(length);
    int len = as_integer(length);
    
    for (i = 0; i < len; i++)
    {
        result->d.v->slots[i] = (TYPE*) obj;
    }

    return result;
}

TYPE* 
list_to_vector(const TYPE* list)
{
    int i;
    int len = length(list);
    TYPE* result = _mk_vector(mk_number_from_int(len));
    
    const TYPE* hd = list;
    for (i = 0; i < len; i++) 
    {
        result->d.v->slots[i] = (TYPE *) car(hd);
        hd = cdr(hd);
    }

    return result;
}

TYPE* 
vector_length(const TYPE* sexp)
{
    assert_throw(is_vector(sexp),
                 TYPE_ERROR,
                 "VECTOR_LENGTH: wrong type in argument");

    return sexp->d.v->length; 
}


TYPE* 
vector_ref(const TYPE* sexp, int k)
{
    assert_throw(is_vector(sexp),
                 TYPE_ERROR,
                 "VECTOR_REF: first argument must be a vector");
    
    assert_throw(k >= 0 && k < as_integer(sexp->d.v->length),
                 TYPE_ERROR,
                 "VECTOR_REF: k is out of range");

    return sexp->d.v->slots[k];
}

void
vector_set(TYPE* sexp, int k, const TYPE* obj)
{
    assert_throw(is_vector(sexp),
                 TYPE_ERROR,
                 "VECTOR_SET: first argument must be a vector");

    assert_throw(k >= 0 && k < as_integer(sexp->d.v->length),
                 TYPE_ERROR,
                 "VECTOR_SET: k is out of range");

    sexp->d.v->slots[k] = (TYPE*) obj;
}


int
vector_eq(const TYPE* left, const TYPE* right)
{
	int result = TRUE;
	const TYPE* length = vector_length(left);
	
	if (is_eqv(length, vector_length(right)))
	{
        int len = as_integer(length);
		for (int i = 0; i < len; i++)
		{
			if (!is_equal(vector_ref(left, i), vector_ref(right, i)))
			{
				result = FALSE;
				break;
			}
		}
	}
	
	return result;
}
