#ifndef _SYMBOL_H_
#define _SYMBOL_H_

#include "type.h"
#define IS_NIL(pointer) (GET_TYPE_TAG(pointer) == NIL_TYPE_TAG)
int _symbol_equal_(const TYPE* left, const TYPE* right);
unsigned int _symbol_hash_(const TYPE* symbol);
void init_symbol_table();
TYPE* mk_symbol(const char* symbol);
#define is_symbol(SEXP) (IS_STRUCT_OF_TYPE(SEXP,SYMBOL))
int is_symbol_eq(const TYPE* left, const TYPE* right);
TYPE* symbol_to_string(const TYPE* symbol);
TYPE* string_to_symbol(const TYPE* symbol);

TYPE* nil();

/* int is_nil(const TYPE* pair);*/

int is_reserved_symbol(const TYPE* symbol);

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

#endif
