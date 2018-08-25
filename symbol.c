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

#define NIL "'()"

static type* symbol_table;
static type* _nil_;

static int
_equal_(const type* left, const type* right)
{
    return strcmp(right->data, left->data) == 0;
}

unsigned int
_symbol_hash_(const type* symbol)
{
    char* s;
    unsigned int result = 0;

    s = (char*) symbol->data;

    for (; *s != 0; s++)
    {
        result = result * 127 + *s;
    }

    return result;
}

void
init_symbol_table()
{
    _nil_ = mloc(sizeof(type));
    
    if (_nil_ == NULL)
    {
        fprintf(stderr, "MK_SYMBOL: could not allocate memory for type");
        exit(1);
    }
        
    _nil_->type = SYMBOL;
    _nil_->data = mloc(sizeof(char) * strlen(NIL));
    
    if (_nil_->data == NULL)
    {
        fprintf(stderr, "MK_SYMBOL: could not allocate memory for symbol");
        exit(1);
    }
    
    strcpy(_nil_->data, NIL);

    symbol_table = mk_hash_table(_equal_, _symbol_hash_);
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
}

type* 
mk_symbol(const char* symbol)
{
    type lookup_symbol;
    type* result;
    type* symbol_in_table;

    /* special handling for nil since hash_table is using it */
    if (strcmp(symbol, "'()") == 0)
    {
        return _nil_;
    }

    lookup_symbol.type = SYMBOL;
    lookup_symbol.data = (void*) symbol; 

    symbol_in_table = hash_table_ref(symbol_table, &lookup_symbol);


    if (is_nil(symbol_in_table))
    {
        result = mloc(sizeof(type));
        
        if (result == NULL)
        {
            fprintf(stderr, "MK_SYMBOL: could not allocate memory for type");
            exit(1);
        }
        
        result->type = SYMBOL;
        result->data = mloc(sizeof(char) * strlen(symbol));
        
        if (result->data == NULL)
        {
            fprintf(stderr, "MK_SYMBOL: could not allocate memory for symbol");
            exit(1);
        }
    
        strcpy(result->data, symbol);
        
        hash_table_set(symbol_table, result, result);
    }
    else
    {
        result = symbol_in_table;
    }

    return result;
}

int 
is_symbol(const type* symbol)
{
    return symbol->type == SYMBOL;
}

int 
is_symbol_eq(const type* left, const type* right)
{
    return left == right;
}

type* 
string_to_symbol(const type* string)
{
    assert_throw(is_string(string),
                 TYPE_ERROR,
                 "STRING_TO_SYMBOL: argument must be a string");

    return mk_symbol(string->data);
}

type* 
nil()
{
    return _nil_;
}

int
is_nil(const type* pair)
{
    return pair == _nil_; 
}

int 
is_reserved_symbol(const type* symbol)
{
    int result = FALSE;

    if (is_eq(_else_keyword_symbol_, symbol) ||
        is_eq(_define_keyword_symbol_, symbol) ||
        is_eq(_unquote_keyword_symbol_, symbol) ||
        is_eq(_unquote_splicing_keyword_symbol_, symbol) ||
        is_eq(_quote_keyword_symbol_, symbol) ||
        is_eq(_lambda_keyword_symbol_, symbol) ||
        is_eq(_if_keyword_symbol_, symbol) ||
        is_eq(_set_keyword_symbol_, symbol) ||
        is_eq(_begin_keyword_symbol_, symbol) ||
        is_eq(_cond_keyword_symbol_, symbol) ||
        is_eq(_and_keyword_symbol_, symbol) ||
        is_eq(_or_keyword_symbol_, symbol) ||
        is_eq(_case_keyword_symbol_, symbol) ||
        is_eq(_match_keyword_symbol_, symbol) ||
        is_eq(_let_keyword_symbol_, symbol) ||
        is_eq(_let_star_keyword_symbol_, symbol) ||
        is_eq(_letrec_keyword_symbol_, symbol) ||
        is_eq(_delay_keyword_symbol_, symbol) ||
        is_eq(_stream_cons_keyword_symbol_, symbol) ||
        is_eq(_quasiquote_keyword_symbol_, symbol))
    {
        result = TRUE;
    }
    
    return result;
}
