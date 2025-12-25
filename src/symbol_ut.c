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
#include <string.h>
#include <assert.h>

#include "symbol.h"
#include "symbol_ut.h"

void tst_symbol()
{
    const char* s1 = "s1";
    const char* s2 = "s2";
        
    TYPE* symbol1 = mk_symbol(s1);
    TYPE* symbol2 = mk_symbol(s2);
    TYPE* symbol3 = mk_symbol(s1);

    assert(!is_symbol_eq(symbol1, symbol2));
    assert(is_symbol_eq(symbol1, symbol3));

    mk_symbol("pair?");
    mk_symbol("cons");
    mk_symbol("car");
    mk_symbol("cdr");
    mk_symbol("null?");
    mk_symbol("number?");
    mk_symbol("complex?");
    mk_symbol("real?");
    mk_symbol("rational?");
    mk_symbol("integer?");
    mk_symbol("exact?");
    mk_symbol("inexact?");
    mk_symbol("=");
    mk_symbol("<");
    mk_symbol(">");
    mk_symbol("<=");
    mk_symbol(">=");
    mk_symbol("zero?");
    mk_symbol("positive?");
    mk_symbol("negative?");
    mk_symbol("odd?");
    mk_symbol("even?");
    mk_symbol("max?");
    mk_symbol("min?");
    mk_symbol("+");
    mk_symbol("*");
    mk_symbol("-");
    mk_symbol("/");
    mk_symbol("abs");
    mk_symbol("quotient");
    mk_symbol("remainder");
    mk_symbol("modulo");
    mk_symbol("gcd");
    mk_symbol("lcm");
    mk_symbol("boolean?");
    mk_symbol("and");
    mk_symbol("or");
    mk_symbol("not");
    mk_symbol("string?");
    mk_symbol("make-string");
    mk_symbol("string");
    mk_symbol("string-length");
    mk_symbol("string-ref");
    mk_symbol("string-set!");
    mk_symbol("string=?");
    mk_symbol("string-ci=?");
    mk_symbol("string<?");
    mk_symbol("string>?");
    mk_symbol("string<=?");
    mk_symbol("string>=?");
    mk_symbol("string-ci<?");
    mk_symbol("string-ci>?");
    mk_symbol("string-ci<=?");
    mk_symbol("string-ci>=?");
    mk_symbol("substring");
    mk_symbol("string-append");
    mk_symbol("string->list");
    mk_symbol("string-copy");
    mk_symbol("string-fill!");
    mk_symbol("vector?");
    mk_symbol("make-vector");
    mk_symbol("vector");
    mk_symbol("vector-length");
    mk_symbol("vector-ref");
    mk_symbol("vector-set!");
    mk_symbol("vector->list");
    mk_symbol("list->vector");
    mk_symbol("vector-fill!");
    mk_symbol("quit");
}
