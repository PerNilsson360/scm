// MIT license
//
// Copyright 2025 Per Nilsson
///
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
