#ifndef _CHAR_H_
#define _CHAR_H_

#include "type.h"

TYPE* mk_char(char c);
int is_char(const TYPE* sexp);
TYPE* char_to_integer(const TYPE* sexp);

#endif
