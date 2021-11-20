#ifndef _CHAR_H_
#define _CHAR_H_

#include "type.h"

TYPE* mk_char(char c);
int is_char(const TYPE* sexp);
int is_char_equal(const TYPE* left, const TYPE* right);
TYPE* char_to_integer(const TYPE* sexp);
TYPE* integer_to_char(const TYPE* sexp);

#endif
