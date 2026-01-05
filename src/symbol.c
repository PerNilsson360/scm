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
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#define GC_DEBUG 1
#include <gc.h>

#include "str.h"
#include "util.h"
#include "common.h"
#include "hash_table.h"
#include "symbol.h"
#include "error.h"

TYPE* _else_keyword_symbol_;
TYPE* _define_keyword_symbol_;
TYPE* _unquote_keyword_symbol_;
TYPE* _unquote_splicing_keyword_symbol_;
TYPE* _quote_keyword_symbol_;
TYPE* _lambda_keyword_symbol_;
TYPE* _if_keyword_symbol_;
TYPE* _set_keyword_symbol_;
TYPE* _begin_keyword_symbol_;
TYPE* _cond_keyword_symbol_;
TYPE* _and_keyword_symbol_;
TYPE* _or_keyword_symbol_;
TYPE* _case_keyword_symbol_;
TYPE* _match_keyword_symbol_;
TYPE* _let_keyword_symbol_;
TYPE* _let_star_keyword_symbol_;
TYPE* _letrec_keyword_symbol_;
TYPE* _delay_keyword_symbol_;
TYPE* _stream_cons_keyword_symbol_;
TYPE* _quasiquote_keyword_symbol_;
TYPE* _call_cc_keyword_symbol_;
TYPE* _apply_keyword_symbol_;

static TYPE* symbol_table;

static
int
_symbol_equal_(const TYPE* left, const TYPE* right)
{
    const char* l = (const char*)REMOVE_TYPE_TAG(left);
    const char* r = (const char*)REMOVE_TYPE_TAG(right);
    return strcmp(l, r) == 0;
}

static
unsigned int
_symbol_hash_(const TYPE* symbol)
{
    unsigned int result = 0;    
    const char* s = (const char*)REMOVE_TYPE_TAG(symbol);
    for (; *s != 0; s++)
    {
        result = result * 127 + *s;
    }
    return result;
}

/* Externaly symbol pointers are unique */
unsigned int
symbol_hash(const TYPE* symbol)
{
    /*
      Lower bits are the same so shift them away.
      TODO: 8 seems to be good but meaybe there is a better way to do this.
     */
    return ((unsigned int) ((intptr_t)symbol)) >> 8;
}

void
init_symbol_table()
{
    symbol_table = mk_hash_table(_symbol_equal_, _symbol_hash_);
    _else_keyword_symbol_ = mk_symbol("else");
    _define_keyword_symbol_ = mk_symbol("define");
    _unquote_keyword_symbol_ = mk_symbol("unquote");
    _unquote_splicing_keyword_symbol_ = mk_symbol("unquote-splicing");
    _quote_keyword_symbol_ = mk_symbol("quote");
    _lambda_keyword_symbol_ = mk_symbol("lambda");
    _if_keyword_symbol_ = mk_symbol("if");
    _set_keyword_symbol_ = mk_symbol("set!");
    _begin_keyword_symbol_ = mk_symbol("begin");
    _cond_keyword_symbol_ = mk_symbol("cond");
    _and_keyword_symbol_ = mk_symbol("and");
    _or_keyword_symbol_ = mk_symbol("or");
    _case_keyword_symbol_ = mk_symbol("case");
    _match_keyword_symbol_ = mk_symbol("match");
    _let_keyword_symbol_ = mk_symbol("let");
    _let_star_keyword_symbol_ = mk_symbol("let*");
    _letrec_keyword_symbol_ = mk_symbol("letrec");
    _delay_keyword_symbol_ = mk_symbol("delay");
    _stream_cons_keyword_symbol_ = mk_symbol("stream-cons");
    _quasiquote_keyword_symbol_ = mk_symbol("quasiquote");
    _call_cc_keyword_symbol_ = mk_symbol("call-with-current-continuation");
    _apply_keyword_symbol_ = mk_symbol("apply");
}

TYPE* 
mk_symbol(const char* symbol)
{
    /* special handling for nil since hash_table is using it */
    if (strcmp(symbol, "'()") == 0)
    {
        return nil();
    }

    TYPE* result;
    TYPE* tmp = mloc(sizeof(char) * strlen(symbol));
    
    if (tmp == NULL)
    {
        fprintf(stderr, "MK_SYMBOL: could not allocate memory for symbol");
        exit(1);
    }
    
    strcpy((char*)tmp, symbol);
    tmp = (TYPE*)(((intptr_t)tmp) | SYMBOL_TYPE_TAG);

    int found = hash_table_ref(symbol_table, tmp, &result);

    if (!found)
    {
        hash_table_set(symbol_table, tmp, tmp);
        result = tmp;
    }

    return result;
}

int 
is_symbol_eq(const TYPE* left, const TYPE* right)
{
    return left == right;
}

TYPE* 
string_to_symbol(const TYPE* string)
{
    assert_throw(is_string(string),
                 TYPE_ERROR,
                 "STRING_TO_SYMBOL: argument must be a string");

    return mk_symbol(string->d.s);
}

TYPE* 
nil()
{
    /* TODO: nil is not a symbol any more */
    return (TYPE*)NIL_TYPE_TAG;
}

int 
is_reserved_symbol(const TYPE* symbol)
{
    return
        is_symbol_eq(_else_keyword_symbol_, symbol) ||
        is_symbol_eq(_define_keyword_symbol_, symbol) ||
        is_symbol_eq(_unquote_keyword_symbol_, symbol) ||
        is_symbol_eq(_unquote_splicing_keyword_symbol_, symbol) ||
        is_symbol_eq(_quote_keyword_symbol_, symbol) ||
        is_symbol_eq(_lambda_keyword_symbol_, symbol) ||
        is_symbol_eq(_if_keyword_symbol_, symbol) ||
        is_symbol_eq(_set_keyword_symbol_, symbol) ||
        is_symbol_eq(_begin_keyword_symbol_, symbol) ||
        is_symbol_eq(_cond_keyword_symbol_, symbol) ||
        is_symbol_eq(_and_keyword_symbol_, symbol) ||
        is_symbol_eq(_or_keyword_symbol_, symbol) ||
        is_symbol_eq(_case_keyword_symbol_, symbol) ||
        is_symbol_eq(_match_keyword_symbol_, symbol) ||
        is_symbol_eq(_let_keyword_symbol_, symbol) ||
        is_symbol_eq(_let_star_keyword_symbol_, symbol) ||
        is_symbol_eq(_letrec_keyword_symbol_, symbol) ||
        is_symbol_eq(_delay_keyword_symbol_, symbol) ||
        is_symbol_eq(_stream_cons_keyword_symbol_, symbol) ||
        is_symbol_eq(_quasiquote_keyword_symbol_, symbol) ||
        is_symbol_eq(_call_cc_keyword_symbol_, symbol);
}
