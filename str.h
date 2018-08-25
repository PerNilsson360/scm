#ifndef _STR_H_
#define _STR_H_

#include "type.h"

int is_string(const type* sexp);
int is_mutable_string(const type* sexp);
type* mk_string(const type* length, const type* character);
type* mk_string_from_chars(const type* char_list);
type* mk_string_with_length(const char* sexp, unsigned int length);
type* string_length(const type* sexp);
type* string_ref(const type* sexp, const type* k);
void string_set(const type* sexp, const type* k, const type* character);
type* string_eq(const type* left, const type* right);
type* string_ci_eq(const type* left, const type* right);
type* string_lt(const type* left, const type* right);
type* string_gt(const type* left, const type* right);
type* string_lt_eq(const type* left, const type* right);
type* string_gt_eq(const type* left, const type* right);
type* string_ci_lt(const type* left, const type* right);
type* string_ci_gt(const type* left, const type* right);
type* string_ci_lt_eq(const type* left, const type* right);
type* string_ci_gt_eq(const type* left, const type* right);
type* substring(const type* sexp, const type* start, const type* end);
type* string_append(const type* left, const type* right);
type* string_to_list(const type* sexp); 
type* list_to_string(const type* sexp); 
type* string_copy(const type* sexp);
type* symbol_to_string(const type* symbol);

#endif
