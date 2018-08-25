#ifndef _VECTOR_H_
#define _VECTOR_H_

#include "type.h"

struct vector
{
    type* length;
    type** slots;
};
typedef struct vector vector;

int is_vector(const type* sexp);
type* mk_vector(const type* length, const type* obj);
type* list_to_vector(const type* list);
type* vector_length(const type* sexp);
type* vector_ref(const type* sexp, int k);
void vector_set(type* sexp, int k, const type* obj);

#endif
