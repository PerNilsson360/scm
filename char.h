#ifndef _CHAR_H_
#define _CHAR_H_

#include "type.h"

type* mk_char(char c);
int is_char(const type* sexp);
type* char_to_integer(const type* sexp);

#endif
