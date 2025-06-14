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


static TYPE* symbol_table;

int
_symbol_equal_(const TYPE* left, const TYPE* right)
{
    return strcmp(right->d.s, left->d.s) == 0;
}

unsigned int
_symbol_hash_(const TYPE* symbol)
{
    char* s;
    unsigned int result = 0;

    s = symbol->d.s;

    for (; *s != 0; s++)
    {
        result = result * 127 + *s;
    }

    return result;
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
    const TYPE lookup_symbol =
    {
	.type = SYMBOL,
	.d.s = (char*) symbol
    };
    
    TYPE* result;
    TYPE* symbol_in_table;

    /* special handling for nil since hash_table is using it */
    if (strcmp(symbol, "'()") == 0)
    {
        return nil();
    }

    int found = hash_table_ref(symbol_table, &lookup_symbol, &symbol_in_table);


    if (!found)
    {
        result = mloc(sizeof(TYPE));
        
        if (result == NULL)
        {
            fprintf(stderr, "MK_SYMBOL: could not allocate memory for type");
            exit(1);
        }
        
        result->type = SYMBOL;
        result->d.s = mloc(sizeof(char) * strlen(symbol));
        
        if (result->d.s == NULL)
        {
            fprintf(stderr, "MK_SYMBOL: could not allocate memory for symbol");
            exit(1);
        }
    
        strcpy(result->d.s, symbol);
        
        hash_table_set(symbol_table, result, result);
    }
    else
    {
        result = symbol_in_table;
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
        is_eq(_quasiquote_keyword_symbol_, symbol) ||
		is_eq(_call_cc_keyword_symbol_, symbol))
    {
        result = TRUE;
    }
    
    return result;
}
