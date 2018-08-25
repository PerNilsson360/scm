#ifndef _UTIL_H_
#define _UTIL_H_

#include "type.h"

type* it_bin_pred(const type* list,
                  int (pred) (const type* left, const type* right));
type* fold_right(type* (* f) (const type* left, const type* right), 
                 const type* list, 
                 type* identity);
type* map1(type* (*f) (const type* arg), type* list);
/* makes a pair of lists out of a association list */
type* unzip(const type* list);
type* reverse(const type* list);
type* append(const type* list, const type* obj);
type* assq(const type* obj, const type* list);
void nstrcpy(char* s1, const char* s2, unsigned int length);
void* mloc(size_t n);
#endif
