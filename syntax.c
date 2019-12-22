#include "type.h"
#include "symbol.h"

#include "syntax.h"

int
is_tagged_list(const TYPE* exp, TYPE* symbol)
{
    return is_pair(exp) && is_eq(car(exp), symbol);
}


TYPE*
mk_lambda(TYPE* parameters, TYPE* body)
{
    return cons(_lambda_keyword_symbol_, cons(parameters, body));
}

TYPE*
mk_if(TYPE* predicate, TYPE* consequent, TYPE* alternative)
{
    return cons(_if_keyword_symbol_,
                cons(predicate,
                     cons(consequent, 
                          cons(alternative, nil()))));
}

TYPE* 
first_exp(TYPE* exps)
{
    return car(exps);
}

int
is_last_exp(TYPE* exps)
{
    return is_nil(cdr(exps));
}

TYPE*
mk_begin(TYPE* exp)
{
    return cons(_begin_keyword_symbol_, exp);
}

int
is_cond(const TYPE* exp)
{
    return is_tagged_list(exp, _cond_keyword_symbol_);
}

TYPE*
cond_clauses(const TYPE* exp)
{
    return cdr(exp);
}

TYPE*
cond_predicate(const TYPE* clause)
{
    return car(clause);
}

TYPE*
cond_actions(const TYPE* clause)
{
    return cdr(clause);
}

int
is_cond_else_clause(const TYPE* clause)
{
    return is_eq(cond_predicate(clause), _else_keyword_symbol_);
}

