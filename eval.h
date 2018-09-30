#ifndef _EVAL_H_
#define _EVAL_H_

#include "type.h"

TYPE* eval(TYPE* sexp, TYPE* env);
TYPE* eval_no_translation(TYPE* sexp, TYPE* env);
int is_tagged_list(const TYPE* exp, TYPE* symbol);

#endif
