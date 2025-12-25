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

#include "common.h"
#include "error.h"
#include "symbol.h"
#include "type.h"
#include "eval.h"
#include "util.h"
#include "io.h"
#include "syntax.h"
#include "str.h"

#include "elab.h"

static
int
is_or(const TYPE* exp)
{
    return is_tagged_list(exp, _or_keyword_symbol_);
}

static
TYPE*
inner_or_to_if(TYPE*exp)
{
    TYPE* result;
    
    if (IS_NIL(exp))
    {
	result = mk_boolean(FALSE);
    }
    else
    {
	TYPE*  xlat_car = xlat(car(exp));
	if (IS_NIL(cdr(exp)))
	{
	    result = mk_sexp_if(xlat_car,
				xlat_car,
				mk_boolean(FALSE));
	}
	else
	{
	    result = mk_sexp_if(xlat_car,
				xlat_car,
				inner_or_to_if(cdr(exp)));
	}
    }

    return result;
}

static
TYPE*
or_to_if(TYPE* exp)
{
    return inner_or_to_if(cdr(exp));
}

static
int
is_and(const TYPE* exp)
{
    return is_tagged_list(exp, mk_symbol("and"));
}

static
TYPE*
inner_and_to_if(TYPE* exp)
{
    TYPE* result;

    if (IS_NIL(exp))
    {
	result = mk_boolean(TRUE);
    }
    else
    {
	TYPE*  xlat_car = xlat(car(exp));
	if (IS_NIL(cdr(exp)))
	{
	    result = mk_sexp_if(xlat_car,
				xlat_car,
				mk_boolean(FALSE));
	}
	else
	{
	    result = mk_sexp_if(xlat_car,
				inner_and_to_if(cdr(exp)),
				mk_boolean(FALSE));
	}
    }
    
    return result;
}

static
TYPE*
and_to_if(TYPE* exp)
{
    return inner_and_to_if(cdr(exp));
}

static
int
is_let(const TYPE* exp)
{
    return is_tagged_list(exp, _let_keyword_symbol_);
}

static
TYPE*
let_to_combination(TYPE* exp)
{
    assert_throw(length(exp) > 2,
		 EVAL_ERROR,
		 "LET_TO_COMBINATION: let must have at least 3 elements");

    TYPE* bindings = cadr(exp);
    TYPE* result;
    TYPE* body = xlat(cddr(exp));

    if (IS_NIL(bindings))
    {
	result = mk_list(1, mk_sexp_lambda(nil(), body)); 
    }
    else if (is_pair(bindings))
    {
	TYPE* unzip_bindings = unzip(bindings);
        
	result = cons(mk_sexp_lambda(car(unzip_bindings), body),
		      xlat(cdr(unzip_bindings)));
    }
    else
    {
	throw_error(EVAL_ERROR,
		    "LET_TO_COMBINATION: bindings must be nil or a pair");
    }
    
    return result;
}

static
int
is_named_let(const TYPE* exp)
{
    return is_tagged_list(exp, mk_symbol("let")) && is_symbol(cadr(exp));
}

static
TYPE*
named_let_to_letrec(TYPE* exp)
{
    TYPE* name;
    TYPE* bindings;
    TYPE* unziped_bindings;
    TYPE* vars;
    TYPE* vals;
    TYPE* body;
    TYPE* result;
   
    assert_throw(length(exp) > 3,
		 EVAL_ERROR,
		 "NAMED_LET_TO_LETREC: letrec must have 4 elements");
    
    bindings = caddr(exp);
    
    assert_throw(is_pair(bindings) || IS_NIL(bindings),
		 EVAL_ERROR,
		 "NAMED_LET_TO_LETREC: 3'd element must be nil or a pair");

    name = cadr(exp);
    unziped_bindings = unzip(bindings);
    vars = car(unziped_bindings);
    vals = cdr(unziped_bindings);
    body = cdddr(exp);

    result = cons(mk_symbol("letrec"),
		  cons(mk_list(1,
			       cons(name,
				    mk_list(1, cons(mk_symbol("lambda"),
						    cons(vars, body))))),
		       mk_list(1, cons(name, vals))));

    return xlat(result);
}

static
int
is_let_star(const TYPE* exp)
{
    return is_tagged_list(exp, mk_symbol("let*"));
}

static
TYPE*
inner_let_star_to_nested_let(const TYPE* bindings,
			     const TYPE* body)
{
    TYPE* result;

    if (length(bindings) == 1)
    {
	result = cons(mk_symbol("let"),
		      cons(mk_list(1, car(bindings)),
			   body));
    }
    else
    {
	result = cons(mk_symbol("let"),
		      cons(mk_list(1, car(bindings)),
			   mk_list(1, inner_let_star_to_nested_let(
				       cdr(bindings),
				       body))));
    }
    
    return xlat(result);
}

static
TYPE*
let_star_to_nested_let(const TYPE* exp)
{
    if (length(exp) < 3)
    {
	throw_error(EVAL_ERROR,
		    "LET_STAR_TO_NESTED_LET: let* must have at least 3 elements");
    }

    return inner_let_star_to_nested_let(cadr(exp), cddr(exp));
}

static
int
is_letrec(const TYPE* exp)
{
    return is_tagged_list(exp, _letrec_keyword_symbol_);
}

static
TYPE*
mk_unassigned_vars(const TYPE* vars)
{
    TYPE* result;
    
    if (IS_NIL(vars))
    {
	result = (TYPE*) vars;
    }
    else
    {
	static const char* s = "*unassigned*";
	TYPE* unassigned = mk_string_with_length(s, strlen(s));
	result = cons(cons(car(vars), mk_list(1, unassigned)),
		      (mk_unassigned_vars(cdr(vars))));
    }
    return result;
}

static
TYPE*
mk_set_var_values(const TYPE* vars, const TYPE* vals)
{
    TYPE* result;
    
    if (IS_NIL(vars))
    {
	result = nil();
    }
    else
    {
	result = cons(cons(mk_symbol("set!"), cons(car(vars), mk_list(1, car(vals)))),
		      mk_set_var_values(cdr(vars), cdr(vals)));
    }
    
    return result;
}

static
TYPE*
letrec_to_let(const TYPE* exp)
{
    TYPE* unziped_bindings = unzip(cadr(exp));
    TYPE* vars = car(unziped_bindings);
    TYPE* vals = cdr(unziped_bindings);
    TYPE* body = xlat(cddr(exp));
    TYPE* result = cons(_let_keyword_symbol_,
			cons(mk_unassigned_vars(vars),
			     append(mk_set_var_values(vars, vals), body)));
    return xlat(result);
}

static
int
is_define(const TYPE*  exp)
{
    int result = FALSE;
    
    if (is_tagged_list(exp, mk_symbol("define")))
    {
	TYPE* arg = cdr(exp);
	if (!IS_NIL(arg))
	{
	    result = !is_symbol(car(arg));
	}
    }
    
    return result;
}

static
TYPE*
define_to_lambda(const TYPE* exp)
{
    if (length(exp) < 3)
    {
	display_debug(exp);
	throw_error(EVAL_ERROR,
		    "DEFINE_TO_LAMBDA: define must have at least 3 elements");
    }
    
    return cons(mk_symbol("define"),
		cons(caadr(exp),
		     mk_list(1, cons(mk_symbol("lambda"),
				     cons(cdadr(exp), xlat(cddr(exp)))))));
}

TYPE*
sequence_to_exp(TYPE* seq)
{
    TYPE* result;

    if (IS_NIL(seq))
    {
        result = nil();
    }
    else if (is_last_exp(seq))
    {
        result = xlat(first_exp(seq));
    }
    else
    {
        result = mk_sexp_begin(xlat(seq));
    }

    return result;
}

static
TYPE*
expand_cond_clauses(const TYPE* exp)
{
    TYPE* result;
    if (IS_NIL(exp))
    {
	result = mk_boolean(FALSE);
    }
    else if (is_cond_else_clause(car(exp)))
    {
	if (!IS_NIL(cdr(exp))) {
	    throw_error(EVAL_ERROR,
			"EXPAND_COND_CLAUSES: else is not last in cond");
	}
	result = sequence_to_exp(cond_actions(car(exp)));
    }
    else
    {
	result = mk_sexp_if(xlat(cond_predicate(car(exp))),
			    sequence_to_exp(cond_actions(car(exp))),
			    xlat(expand_cond_clauses(cdr(exp))));
    }

    return result;
}

static
TYPE*
cond_to_if(const TYPE* exp)
{
    return expand_cond_clauses(cond_clauses(exp));
}

static
int
is_case(const TYPE* exp)
{
    return is_tagged_list(exp,_case_keyword_symbol_);
}

static
TYPE*
case_key(const TYPE* exp)
{
    return cadr(exp);
}

static
TYPE*
case_clauses(const TYPE* exp)
{
    return cddr(exp);
}

static
TYPE*
case_clause_datum(const TYPE* clause)
{
    return car(clause);
}

static
TYPE*
case_clause_exp(const TYPE* clause)
{
    return cdr(clause);
}

static
int
is_last_clause(const TYPE* clause)
{
    return IS_NIL(cdr(clause));
}

static
int
is_else_clause(const TYPE* clause)
{
    return is_eq(case_clause_datum(clause), _else_keyword_symbol_);
}

static
TYPE*
inner_construct_case_clauses(const TYPE* clauses)
{
    TYPE* result;
    
    if (IS_NIL(clauses))
    {
	result = nil();
    }
    else if (is_else_clause(car(clauses)))
    {
	assert_throw(is_last_clause(clauses),
		     CONSTRAINT_ERROR,
		     "CONSTRUCT_CASE_CLAUSES: else must be last");
	result = (TYPE*)clauses;
    }
    else
    {
	TYPE* first_clause = car(clauses);
	result = cons(cons(cons(mk_symbol("memv"),
				cons(mk_symbol("key"),
				     mk_list(1,
					     mk_list(2,
						     _quote_keyword_symbol_,
						     case_clause_datum(first_clause))))),
			   case_clause_exp(first_clause)),
		      inner_construct_case_clauses(cdr(clauses)));
    }
    
    return result;
}

static
TYPE*
construct_case_clauses(const TYPE* clauses)
{
    assert_throw(!IS_NIL(clauses) && length(clauses) > 0,
		 CONSTRAINT_ERROR,
		 "CONSTRUCT_CASE_CLAUSES: must contain at least one clause");
    return cons(_cond_keyword_symbol_, inner_construct_case_clauses(clauses));
}

static
TYPE*
case_to_cond(const TYPE* exp)
{
    // need to xlat since we are translating to let
    TYPE* result = mk_list(3,
			   _let_keyword_symbol_,
			   mk_list(1, mk_list(2, mk_symbol("key"), case_key(exp))),
			   construct_case_clauses(case_clauses(exp)));
    return xlat(result);
}

static
int
is_quasiquote(const TYPE* exp)
{
    return is_tagged_list(exp, _quasiquote_keyword_symbol_);
}

static
int
is_unqoute_splicing(const TYPE* exp)
{
    return is_tagged_list(exp, _unquote_splicing_keyword_symbol_);
}

static
int
is_unquote(const TYPE* exp)
{
    return is_tagged_list(exp, _unquote_keyword_symbol_);
}

static
TYPE*
unquote_value(const TYPE* exp)
{
    return cadr(exp);
}

static
TYPE*
quasiquote_to_list(const TYPE* exp)
{
    TYPE*  result;
    
    if (IS_NIL(exp))
    {
	result = (TYPE*) exp;
    }
    else if (is_unquote(exp))
    {
	result = unquote_value(exp);
    }
    else if (is_symbol(exp))
    {
	result = mk_list(2, _quote_keyword_symbol_, exp);
    }
    else if (is_pair(exp))
    {
	TYPE* first = car(exp);
	TYPE* rest = cdr(exp);
        
	if (is_unqoute_splicing(first))
	{
	    result = cons(mk_symbol("append"),
			  cons(unquote_value(first),
			       mk_list(1, quasiquote_to_list(rest))));
	}
	else
	{
	    result = cons(mk_symbol("cons"),
			  cons(quasiquote_to_list(first),
			       mk_list(1, quasiquote_to_list(rest))));
	}
    }
    else
    {
	result = (TYPE*)exp;
    }

    return result;
}

static
TYPE*
xlat_sequence(const TYPE* exp)
{
    TYPE* result;
    
    if (IS_NIL(exp))
    {
	result = (TYPE*)exp;
    }
    else if (is_pair(exp))
    {
	result = cons(xlat(car(exp)), xlat_sequence(cdr(exp)));
    }
    else
    {
	result = xlat((TYPE*)exp); /* @todo remove cast */
    }

    return result;
}

TYPE*
xlat(TYPE* exp)
{
    TYPE* result;
    if (is_sexp_quoted(exp))
    {
	assert_throw(length(exp) == 2,
		     EVAL_ERROR,
		     "XLAT: quote must have 1 operand");
	result = mk_quoted(sexp_quotation_value(exp));
    }
    else if (!is_pair(exp)) {
	result = exp;
    }
    /* all the following are compound expressions */
    else if (is_or(exp))
    {
	result = or_to_if(exp);
    }
    else if (is_and(exp))
    {
	result = and_to_if(exp);
    }
    else if (is_named_let(exp))
    {
	result = named_let_to_letrec(exp);
    }
    else if (is_let(exp))
    {
	result = let_to_combination(exp);
    }
    else if (is_let_star(exp))
    {
	result = let_star_to_nested_let(exp);
    }
    else if (is_letrec(exp))
    {
	result = letrec_to_let(exp);
    }
    else if (is_define(exp))
    {
	result = define_to_lambda(exp);
    }
    else if (is_cond(exp))
    {
	result = cond_to_if(exp);
    }
    else if (is_case(exp))
    {
	result = case_to_cond(exp);
    }
    else if (is_quasiquote(exp))
    {
	result = quasiquote_to_list(cdr(exp));
    }
    /* a body expression */
    else if (is_pair(exp))
    {
	result = xlat_sequence(exp);
    }
    else
    {
	/* No translation */
	result = exp;
    }

    return result;
}
