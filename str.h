#ifndef _STR_H_
#define _STR_H_

#include "type.h"

int is_string(const TYPE* sexp);
int is_mutable_string(const TYPE* sexp);
TYPE* mk_string(const TYPE* length, const TYPE* character);
TYPE* mk_string_from_chars(const TYPE* char_list);
TYPE* mk_string_with_length(const char* sexp, unsigned int length);
TYPE* string_length(const TYPE* sexp);
TYPE* string_ref(const TYPE* sexp, const TYPE* k);
void string_set(const TYPE* sexp, const TYPE* k, const TYPE* character);
TYPE* string_eq(const TYPE* left, const TYPE* right);
TYPE* string_ci_eq(const TYPE* left, const TYPE* right);
TYPE* string_lt(const TYPE* left, const TYPE* right);
TYPE* string_gt(const TYPE* left, const TYPE* right);
TYPE* string_lt_eq(const TYPE* left, const TYPE* right);
TYPE* string_gt_eq(const TYPE* left, const TYPE* right);
TYPE* string_ci_lt(const TYPE* left, const TYPE* right);
TYPE* string_ci_gt(const TYPE* left, const TYPE* right);
TYPE* string_ci_lt_eq(const TYPE* left, const TYPE* right);
TYPE* string_ci_gt_eq(const TYPE* left, const TYPE* right);
TYPE* substring(const TYPE* sexp, const TYPE* start, const TYPE* end);
TYPE* string_append(const TYPE* left, const TYPE* right);
TYPE* string_to_list(const TYPE* sexp); 
TYPE* list_to_string(const TYPE* sexp); 
TYPE* string_copy(const TYPE* sexp);
TYPE* symbol_to_string(const TYPE* symbol);

#endif
