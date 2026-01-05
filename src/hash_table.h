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
#ifndef _HASH_TABLE_H_
#define _HASH_TABLE_H_

#include "type.h"

#define HASH_TABLE_DEFAULT_SIZE 4391 /* A prime number */

TYPE* mk_hash_table(int (*equal) (const TYPE* left, const TYPE* right),
		    unsigned int (*hash) (const TYPE* key));

int hash_table_ref(const TYPE* hash_table, const TYPE* key, TYPE** result);
void hash_table_delete(TYPE* hash_table, const TYPE* key);
void hash_table_set(TYPE* hash_table, const TYPE* key, const TYPE* data);

#endif
