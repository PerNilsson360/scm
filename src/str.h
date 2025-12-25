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
TYPE* string_to_number(const TYPE* sexp, const TYPE* radix); 
TYPE* string_copy(const TYPE* sexp);
TYPE* symbol_to_string(const TYPE* symbol);

#endif
