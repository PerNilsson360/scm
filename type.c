#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdarg.h>
#include <assert.h>

#include <gc.h>

#include "io.h"
#include "symbol.h"
#include "number.h"
#include "char.h"
#include "str.h"
#include "vector.h"
#include "common.h"
#include "error.h"
#include "type.h"
#include "eval.h"
#include "util.h"
#include "syntax.h"

TYPE* 
mk_bound_var(TYPE* symbol, 
             unsigned int frame_index, 
             unsigned int var_index,
             int is_inproper_list)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_BOUND_VAR: could not allocate memory for type");
        exit(1);
    }

    result->type = BOUND_VAR;
    result->d.b = mloc(sizeof(BOUND_VAR_DATA));
    
    if (result->d.b == NULL)
    {
        fprintf(stderr, "MK_VAR_VAR: could not allocate memory for data");
        exit(1);
    }

    result->d.b->symbol = symbol;
    result->d.b->frame_index = frame_index;
    result->d.b->var_index = var_index;
    result->d.b->is_inproper_list = is_inproper_list;

    return result;
}

int is_bound_var(const TYPE* exp)
{
    return exp->type == BOUND_VAR;
}

TYPE* 
mk_none()
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_NONE: could not allocate memory for type");
        exit(1);
    }

    result->type = NONE;
    result->d.i = 0;

    return result;
}

int 
is_none(const TYPE* sexp)
{
    return sexp->type == NONE;
}

static
TYPE* 
mk_cell_data(int type, const TYPE* sexp)
{
	TYPE* result = mloc(sizeof(TYPE));
	   
    if (result == NULL)
    {
        fprintf(stderr, "MK_CELL_DATA: could not allocate memory for type");
        exit(1);
    }

	result->type = type;
	result->d.t = (TYPE*)sexp;
	
    return result;
}

static
TYPE*
mk_pair_data(int type, const TYPE* car, const TYPE* cdr)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_PAIR_DATA: could not allocate memory for type");
        exit(1);
    }

    result->type = type;
    result->d.p = mloc(sizeof(PAIR_DATA));

    if (result->d.p == NULL)
    {
        fprintf(stderr, "MK_PAIR_DATA: could not allocate memory for data");
        exit(1);
    }

    result->d.p->car = (TYPE*) car;
    result->d.p->cdr = (TYPE*) cdr;

    return result; 
	
}

TYPE* 
cons(const TYPE* car, const TYPE* cdr)
{
	return mk_pair_data(PAIR, car, cdr);
}

TYPE* 
car(const TYPE* list)
{
    if (list->type != PAIR)
    {
        display_debug(list);
        printf("not a pair\n");
        /*assert_throw(FALSE, TYPE_ERROR, "CAR: not a pair");*/
		assert(0);  
    }

    return list->d.p->car;
}

TYPE* 
cdr(const TYPE* list)
{
    if (list->type != PAIR)
    {
        display_debug(list);
        printf("not a pair\n");
        assert_throw(FALSE, TYPE_ERROR, "CDR: not a pair");
    }

    return list->d.p->cdr;
}

TYPE*
caar(const TYPE* list)
{
    return car(car(list));
}

TYPE*
cadr(const TYPE* list)
{
    return car(cdr(list));
}

TYPE*
caadr(const TYPE* list)
{
    return car(car(cdr(list)));
}

TYPE*
caddr(const TYPE* list)
{
    return car(cdr(cdr(list)));
}

TYPE*
cddr(const TYPE* list)
{
    return cdr(cdr(list));
}

TYPE*
cdar(const TYPE* list)
{
    return cdr(car(list));
}

TYPE*
cdadr(const TYPE* list)
{
    return cdr(car(cdr(list)));
}

TYPE*
cdddr(const TYPE* list)
{
    return cdr(cdr(cdr(list)));
}

TYPE*
cadddr(const TYPE* list)
{
    return car(cdr(cdr(cdr(list))));
}

void 
set_car(TYPE* list, const TYPE* value)
{
    assert_throw(is_pair(list), TYPE_ERROR, "SET-CAR: not a pair");

    list->d.p->car = (TYPE*)value;
}

void 
set_cdr(TYPE* list, const TYPE* value)
{
    assert_throw(is_pair(list), TYPE_ERROR, "SET-CDR: not a pair");
    
    list->d.p->cdr = (TYPE*)value;
}

int
is_list(const TYPE* sexp)
{
    int result;

    if (IS_NIL(sexp))
    {
        result = 1;
    }
    else if (!is_pair(sexp))
    {
        result = 0;
    }
    else
    {
        result = is_list(cdr(sexp));
    }

    return result;
}

TYPE*
list(const TYPE* sexp)
{
    return (TYPE*) sexp;
}

TYPE*
mk_list(int count, ...)
{
    va_list ap;
    int i;
    TYPE* result = nil();

    va_start(ap, count);

    for (i = 0; i < count; i++)
    {
		result = cons(va_arg(ap, TYPE*), result);
    }
    
    va_end(ap);

    return reverse(result);
}

int 
is_empty_pair(const TYPE* sexp)
{
    return is_pair(sexp) && IS_NIL(car(sexp)) && IS_NIL(cdr(sexp));
}

unsigned int 
length(const TYPE* pair)
{
    unsigned int result = 0;

	assert_throw(is_pair(pair) || IS_NIL(pair), TYPE_ERROR, "LENGTH: not a pair");

    if (!IS_NIL(pair))
    {
        result = 1 + length(cdr(pair));
    }
    
    return result;
}

int
is_procedure(const TYPE* proc)
{
	return proc->type == PROCEDURE || proc->type == PRIMITIVE_PROCEDURE;
}

int
is_eqv(const TYPE* left, const TYPE* right)
{
	if (left->type != right->type) {
		return FALSE;
	}
    switch (left->type) {
	case INTEGER:
	case RATIONAL:
	case REAL:
	case COMPLEX:
		return is_number_equal(left, right);
	case CHAR:
		return is_char_equal(left, right);
	case BOOLEAN:
		return left->d.i == right->d.i;
	case SYMBOL:
        return left->d.s == right->d.s;
	default:
		return left == right;
	}
}

int
is_equal(const TYPE* left, const TYPE* right)
{
    int result = FALSE;
    if (is_pair(left) && is_pair(right))
    {
        result = is_equal(car(left), car(right)) && is_equal(cdr(left), cdr(right));
    }
	else if (is_string(left) && is_string(right))
	{
		result = is_true(string_eq(left, right));
	}
	else if (is_vector(left) && is_vector(right))
	{
		result = vector_eq(left, right);
	}
    else
    {
        result = is_eqv(left, right);
    }
    
    return result;
}

TYPE* 
mk_sexp_quoted(const TYPE* sexp)
{
	return cons(_quote_keyword_symbol_, cons(sexp, nil()));
}

TYPE* 
mk_quoted(const TYPE* sexp)
{
	return mk_cell_data(QUOTE, sexp);
}

int 
is_sexp_quoted(const TYPE* sexp)
{
    return is_tagged_list(sexp, _quote_keyword_symbol_);
}

TYPE* 
sexp_quotation_value(const TYPE* sexp)
{
    return car(cdr(sexp));
}

TYPE* 
mk_boolean(int t)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_BOOLEAN: could not allocate memory for type");
        exit(1);
    }

    result->type = BOOLEAN;
    
    result->d.i = t;
    
    return result;
}

int 
is_boolean(const TYPE* sexp)
{
    return sexp->type == BOOLEAN;
}

TYPE* 
not(const TYPE* sexp)
{
    return mk_boolean(!is_true(sexp));
}

int 
is_true(const TYPE* sexp)
{
    return !(sexp != NULL && 
             sexp->type == BOOLEAN && 
             sexp->d.i == FALSE);
}

int
is_eof_object(const TYPE* sexp)
{
    return sexp->type == ENDOFFILE;
}

TYPE*
mk_eof()
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_EOF: could not allocate memory for type");
        exit(1);
    }

    result->type = ENDOFFILE;

    return result;
}

TYPE*
mk_assignment(TYPE* parameters, TYPE* body)
{
	return mk_pair_data(ASSIGNMENT, parameters, body);
}

TYPE*
mk_definition(TYPE* var, TYPE* value)
{
	return mk_pair_data(DEFINITION, var, value);
}

TYPE*
mk_lambda(TYPE* parameters, TYPE* body)
{
	return mk_pair_data(LAMBDA, parameters, body);
}

TYPE*
mk_begin(TYPE* actions)
{
	return mk_cell_data(BEGIN_TYPE, actions);
}

TYPE*
mk_match(TYPE* key, TYPE* clauses)
{
	return mk_pair_data(MATCH, key, clauses);
}

TYPE*
mk_delay(TYPE* actions)
{
	return mk_cell_data(DELAY, actions);
}

TYPE* mk_call_cc(TYPE* escape_procedure)
{
	return mk_cell_data(CALL_CC, escape_procedure);
}

TYPE* mk_apply(TYPE* procedure, TYPE* arguments)
{
	return mk_pair_data(APPLY, procedure, arguments);
}

TYPE*
mk_if(TYPE* predicate, TYPE* consequent, TYPE* alternative)
{
	TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_IF: could not allocate memory for type");
        exit(1);
    }
    
    result->type = IF_TYPE;
	result->d.ifd = mloc(sizeof(IF_DATA));

    if (result->d.ifd == NULL)
    {
        fprintf(stderr, "MK_IF: could not allocate memory for data");
        exit(1);
    }
	
	result->d.ifd->predicate = predicate;
	result->d.ifd->consequent = consequent;
	result->d.ifd->alternative = alternative;
	
    return result;
}

int
is_escape_proc(const TYPE* sexp)
{
	return sexp->type == ESCAPE_PROC;
}

TYPE*
mk_escape_proc(const STACK* stack, const REGS* regs)
{
	TYPE* result = mloc(sizeof(TYPE));
	   
    if (result == NULL)
    {
        fprintf(stderr, "MK_ESCAPE_PROC: could not allocate memory for type");
        exit(1);
    }

	result->type = ESCAPE_PROC;
	result->d.e = mloc(sizeof(ESCAPE_PROC_DATA));
	
	if (result->d.e == NULL)
    {
        fprintf(stderr, "MK_ESCAPE_PROC: could not allocate memory for escape proc data");
        exit(1);
    }

	copy_stack(&(result->d.e->stack), stack);
	memcpy(&(result->d.e->regs), regs, sizeof(REGS));
	return result;
}

void copy_regs(REGS* dest, const REGS* src) {
	memcpy(dest, src, sizeof(REGS));
}
