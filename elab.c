#include "common.h"
#include "error.h"
#include "symbol.h"
#include "type.h"
#include "eval.h"
#include "util.h"
#include "io.h"

#include "elab.h"

static
int
is_or(const TYPE* exp)
{
    return is_tagged_list(exp, mk_symbol("or"));
}

static
TYPE*
inner_or_to_if(TYPE*exp)
{
    TYPE* result;
    
    if (is_nil(exp))
    {
	result = mk_boolean(FALSE);
    }
    else
    {
	TYPE* or_to_if_var_symbol = mk_symbol("or->if-var");
	result = mk_list(1,
			 mk_list(2,
				 mk_symbol("lambda"),
				 mk_list(2,
					 or_to_if_var_symbol,
					 mk_list(4,
						 mk_symbol("if"),
						 or_to_if_var_symbol,
						 or_to_if_var_symbol,
						 inner_or_to_if(cdr(exp))))),
			 xlat(car(exp)));
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

    if (is_nil(exp))
    {
	result = mk_boolean(TRUE);
    }
    else
    {
	TYPE*  xlat_car = xlat(car(exp));
	if (is_nil(cdr(exp)))
	{
	    result = mk_list(4,
			     mk_symbol("if"),
			     xlat_car,
			     xlat_car,
			     mk_boolean(FALSE));
	}
	else
	{
	    result = mk_list(4,
			     mk_symbol("if"),
			     xlat_car,
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
    return is_tagged_list(exp, mk_symbol("let"));
}

static
TYPE*
let_to_combination(TYPE* exp)
{
    TYPE* bindings;
    TYPE* result;
    
    if (length(exp) < 3)
    {
	throw_error(EVAL_ERROR,
		    "LET_TO_COMBINATION: let must have at least 3 elements");
    }

    bindings = cadr(exp);

    if (is_nil(bindings))
    {
	result = xlat(caddr(exp));
    }
    else if (is_pair(bindings))
    {
	TYPE* body = xlat(cddr(exp));
	TYPE* unzip_bindings = unzip(bindings);
	
	result = cons(cons(mk_symbol("lambda"),
			   cons(car(unzip_bindings), body)),
		      xlat(cdr(unzip_bindings)));
    }
    else
    {
	throw_error(EVAL_ERROR,
		    "LET_TO_COMBINATION: bindings must be nil or a pair");
    }
    
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
   
    if (length(exp) < 4)
    {
	throw_error(EVAL_ERROR,
		    "NAMED_LET_TO_LETREC: letrec must have 4 elements");

    }

    bindings = caddr(exp);

    if (is_nil(bindings))
    {
	result = xlat(car(cdddr(exp)));
    }
    else if (is_pair(bindings))
    {
	unziped_bindings = unzip(bindings);
	vars = car(unziped_bindings);
	vals = cdr(unziped_bindings);
	body = cdddr(exp);
		    
	result = cons(mk_symbol("letrec"),
		      cons(mk_list(2,
				   cons(name,
					mk_list(1, cons(mk_symbol("lambda"),
							cons(vars, body))))),
			   mk_list(1, cons(name, vals))));
							    
    }
    else
    {
	throw_error(EVAL_ERROR,
		    "NAMED_LET_TO_LETREC: 3'd element must be nil or a pair");
    }

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

    if (length(bindings))
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
let_star_to_nested_let(TYPE* exp)
{
    if (length(exp) < 3)
    {
	throw_error(EVAL_ERROR,
		    "LET_STAR_TO_NESTED_LET: let* must have at least 3 elements");
    }

    
    return inner_let_star_to_nested_let(cadr(exp), cddr(exp));
}


TYPE*
xlat(TYPE* exp)
{
    TYPE* result;
    
    if (is_quoted(exp) || !is_pair(exp))
    {
	result = exp;
    }
    /* all the following are compound expressions */
    else if (is_or(exp))
    {
	result = or_to_if(exp);
    }
    else if (is_let(exp))
    {
	result = let_to_combination(exp);
    }
    else if (is_named_let(exp))
    {
	result = named_let_to_letrec(exp);
    }
    else if (is_let_star(exp))
    {
	result = let_star_to_nested_let(exp);
    }
    else
    {
	display_debug(exp);
	throw_error(EVAL_ERROR, "XLAT: unkown expression");
    }

    return result;
}
