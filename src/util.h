#ifndef _UTIL_H_
#define _UTIL_H_

#include "type.h"

#define MAX_IDENTIFIER_LENGTH 1024

TYPE* it_bin_pred(const TYPE* list,
                  int (pred) (const TYPE* left, const TYPE* right));
TYPE* fold_right(TYPE* (* f) (const TYPE* left, const TYPE* right), 
                 const TYPE* list, 
                 TYPE* identity);
TYPE* map1(TYPE* (*f) (const TYPE* arg), TYPE* list);
/* makes a pair of lists out of a association list */
TYPE* unzip(const TYPE* list);
TYPE* reverse(const TYPE* list);
TYPE* append(const TYPE* list, const TYPE* obj);
TYPE* assq(const TYPE* obj, const TYPE* list);
void nstrcpy(char* s1, const char* s2, unsigned int length);
void* mloc(size_t n);
#endif
