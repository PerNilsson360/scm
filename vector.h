#ifndef _VECTOR_H_
#define _VECTOR_H_

#include "type.h"

int is_vector(const TYPE* sexp);
TYPE* mk_vector(const TYPE* length, const TYPE* obj);
TYPE* list_to_vector(const TYPE* list);
TYPE* vector_length(const TYPE* sexp);
TYPE* vector_ref(const TYPE* sexp, int k);
void vector_set(TYPE* sexp, int k, const TYPE* obj);

#endif
