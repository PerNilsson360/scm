#ifndef _EVAL_H_
#define _EVAL_H_

#include "type.h"

type* eval(type* sexp, type* env);
type* eval_no_translation(type* sexp, type* env);
int is_tagged_list(const type* exp, type* symbol);


#endif
