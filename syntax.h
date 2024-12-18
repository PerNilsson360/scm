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
