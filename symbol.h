#ifndef _SYMBOL_H_
#define _SYMBOL_H_

#include "type.h"
unsigned int _symbol_hash_(const type* symbol);
void init_symbol_table();
type* mk_symbol(const char* symbol);
int is_symbol(const type* symbol);
int is_symbol_eq(const type* left, const type* right);
type* symbol_to_string(const type* symbol);
type* string_to_symbol(const type* symbol);

type* nil();
int is_nil(const type* pair);

int is_reserved_symbol(const type* symbol);

type* _else_keyword_symbol_;
type* _define_keyword_symbol_;
type* _unquote_keyword_symbol_;
type* _unquote_splicing_keyword_symbol_;
type* _quote_keyword_symbol_;
type* _lambda_keyword_symbol_;
type* _if_keyword_symbol_;
type* _set_keyword_symbol_;
type* _begin_keyword_symbol_;
type* _cond_keyword_symbol_;
type* _and_keyword_symbol_;
type* _or_keyword_symbol_;
type* _case_keyword_symbol_;
type* _match_keyword_symbol_;
type* _let_keyword_symbol_;
type* _let_star_keyword_symbol_;
type* _letrec_keyword_symbol_;
type* _delay_keyword_symbol_;
type* _stream_cons_keyword_symbol_;
type* _quasiquote_keyword_symbol_;

#endif
