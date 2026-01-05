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
#ifndef _SYMBOL_H_
#define _SYMBOL_H_

#include "type.h"

#define IS_NIL(pointer) (GET_TYPE_TAG(pointer) == NIL_TYPE_TAG)
unsigned int symbol_hash(const TYPE* symbol);
void init_symbol_table();
TYPE* mk_symbol(const char* symbol);
#define IS_SYMBOL(POINTER) (IS_TAGGED_POINTER_OF_TYPE(POINTER, SYMBOL_TYPE_TAG))
int is_symbol_eq(const TYPE* left, const TYPE* right);
TYPE* symbol_to_string(const TYPE* symbol);
TYPE* string_to_symbol(const TYPE* symbol);

TYPE* nil();

/* int is_nil(const TYPE* pair);*/

int is_reserved_symbol(const TYPE* symbol);

extern TYPE* _else_keyword_symbol_;
extern TYPE* _define_keyword_symbol_;
extern TYPE* _unquote_keyword_symbol_;
extern TYPE* _unquote_splicing_keyword_symbol_;
extern TYPE* _quote_keyword_symbol_;
extern TYPE* _lambda_keyword_symbol_;
extern TYPE* _if_keyword_symbol_;
extern TYPE* _set_keyword_symbol_;
extern TYPE* _begin_keyword_symbol_;
extern TYPE* _cond_keyword_symbol_;
extern TYPE* _and_keyword_symbol_;
extern TYPE* _or_keyword_symbol_;
extern TYPE* _case_keyword_symbol_;
extern TYPE* _match_keyword_symbol_;
extern TYPE* _let_keyword_symbol_;
extern TYPE* _let_star_keyword_symbol_;
extern TYPE* _letrec_keyword_symbol_;
extern TYPE* _delay_keyword_symbol_;
extern TYPE* _stream_cons_keyword_symbol_;
extern TYPE* _quasiquote_keyword_symbol_;
extern TYPE* _call_cc_keyword_symbol_;
extern TYPE* _apply_keyword_symbol_;

#endif
