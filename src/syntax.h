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
#ifndef _SYNTAX_H_
#define _SYNTAX_H_

int is_tagged_list(const TYPE* exp, TYPE* symbol);

#define IS_SEXP_ASSIGNMENT(exp) is_tagged_list(exp, _set_keyword_symbol_)
#define SEXP_ASSIGNMENT_VARIABLE(exp) car(cdr(exp))
#define SEXP_ASSIGNMENT_VALUE(exp) car(cdr(cdr(exp)))

TYPE* mk_sexp_lambda(TYPE* parameters, TYPE* body);
TYPE* mk_sexp_if(TYPE* predicate, TYPE* consequent, TYPE* alternative);
TYPE* first_exp(TYPE* exps);
int is_last_exp(TYPE* exps);
TYPE* mk_sexp_begin(TYPE* exp);

int is_cond(const TYPE* exp);
TYPE* cond_clauses(const TYPE* exp);
TYPE* cond_predicate(const TYPE* exp);
TYPE* cond_actions(const TYPE* exp);
int is_cond_else_clause(const TYPE* exp);

#endif
