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
#ifndef _NUMBER_H_
#define _NUMBER_H_

#include "type.h"

TYPE* mk_unasigned_number(int type);
TYPE* mk_number_from_int(int n);
TYPE* mk_number(const char* number, int radix);
TYPE* mk_real(const char* number);
int is_number(const TYPE* number);
int is_integer(const TYPE* number);
int is_real(const TYPE* number);
int is_number_equal(const TYPE* left, const TYPE* right);
int is_number_lt(const TYPE* left, const TYPE* right);
int is_number_gt(const TYPE* left, const TYPE* right);
int is_number_lt_eq(const TYPE* left, const TYPE* right);
int is_number_gt_eq(const TYPE* left, const TYPE* right);
TYPE* is_number_zero(const TYPE* left);
TYPE* is_number_positive(const TYPE* left);
TYPE* is_number_negative(const TYPE* left);
TYPE* is_number_odd(const TYPE* left);
TYPE* is_number_even(const TYPE* left);
TYPE* max_number(const TYPE* left, const TYPE* right);
TYPE* min_number(const TYPE* left, const TYPE* right);
TYPE* add_number(const TYPE* left, const TYPE* right);
TYPE* mul_number(const TYPE* left, const TYPE* right);
TYPE* sub_numbers(const TYPE* numbers);
TYPE* div_numbers(const TYPE* numbers);
TYPE* remainder_number(const TYPE* left, const TYPE* right);
TYPE* round_number(const TYPE* number);
TYPE* number_to_string(const TYPE* n);

unsigned int number_hash(const TYPE* number);
int as_integer(const TYPE* number);
#endif
