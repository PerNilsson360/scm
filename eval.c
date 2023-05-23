#include <stdio.h>
#include <string.h>

#include "error.h"
#include "util.h"
#include "common.h"
#include "type.h"
#include "number.h"
#include "symbol.h"
#include "str.h"
#include "procedure.h"
#include "primitive_procedure.h"
#include "eval.h"
#include "env.h"
#include "stack.h"
#include "io.h"
#include "char.h"
#include "vector.h"
#include "syntax.h"
#include "elab.h"

extern int _debug_;             /* defined in primitive_procedure */
static REGS reg;

/* Syntax procedures */

static
inline
int
is_self_evaluating(const TYPE* sexp)
{
    return 
        is_number(sexp) ||
        is_char(sexp) ||
        is_boolean(sexp) ||
        is_string(sexp) ||
        is_vector(sexp) ||
        is_none(sexp) ||
        IS_NIL(sexp) ||
		is_escape_proc(sexp);
}

static
inline
int
is_variable(const TYPE* exp)
{
    return is_symbol(exp) || is_bound_var(exp);
}

int
is_assignment(TYPE* exp)
{
    return is_tagged_list(exp, _set_keyword_symbol_);
}

TYPE*
assignment_variable(TYPE* exp)
{
    return car(cdr(exp));
}

TYPE*
assignment_value(TYPE* exp)
{
    return car(cdr(cdr(exp)));
}

int
is_definition(TYPE* exp)
{
    return is_tagged_list(exp, _define_keyword_symbol_);
}

TYPE*
definition_variable(TYPE* exp)
{
    return car(cdr(exp));
}

TYPE*
mk_definition(TYPE* var, TYPE* value)
{
    cons(_define_keyword_symbol_, cons(var, cons(value, nil())));
}

TYPE*
definition_value(TYPE* exp)
{
    return car(cdr(cdr(exp)));
}

int
is_lambda(TYPE* exp)
{
    return is_tagged_list(exp, _lambda_keyword_symbol_);
}

TYPE* 
lambda_parameters(TYPE* exp)
{
    return car(cdr(exp));
}

TYPE*
lambda_body(TYPE* exp)
{
    return cdr(cdr(exp));
}

int
is_if(TYPE* exp)
{
    return is_tagged_list(exp, _if_keyword_symbol_);
}

TYPE* 
if_predicate(TYPE* exp)
{
    return car(cdr(exp));
}

TYPE*
if_consequent(TYPE* exp)
{
    return car(cdr(cdr(exp)));
}

TYPE*
if_alternative(TYPE* exp)
{
    TYPE* result = mk_none();
    
    if (!IS_NIL(cdr(cdr(cdr(exp)))))
    {
        result = car(cdr(cdr(cdr(exp))));
    }

    return result;
}


int
is_begin(TYPE* exp)
{
    return is_tagged_list(exp, _begin_keyword_symbol_);
}

TYPE* 
begin_actions(TYPE* exp)
{
    return cdr(exp);
}

TYPE* 
rest_exps(TYPE* exps)
{
    return cdr(exps);
}

int 
is_application(const TYPE* sexp)
{
    return is_pair(sexp);
}

TYPE* 
operator(TYPE* exp)
{
    return car(exp);
}

static TYPE*
operands(TYPE* exp)
{
    return cdr(exp);
}

static TYPE*
first_operand(TYPE* operands)
{
    return car(operands);
}

static int
last_operand(TYPE* operands)
{
    return IS_NIL(cdr(operands));
}

static int 
no_operands(TYPE* operands)
{
    return IS_NIL(operands);
}

static TYPE*
rest_operands(TYPE* operands)
{
    return cdr(operands);
}

static int
is_match(const TYPE* exp)
{
    return is_tagged_list(exp, _match_keyword_symbol_);
}

static TYPE*
mk_match(TYPE* key, TYPE* clauses)
{
    return cons(_match_keyword_symbol_, cons(key, clauses));
}


static TYPE*
match_key(TYPE* exp)
{
    return car(cdr(exp));
}

static TYPE*
match_clauses(TYPE* exp)
{
    return cdr(cdr(exp));
}

static TYPE*
find(const TYPE* var, TYPE* vars, TYPE* vals)
{
    if (IS_NIL(vars)) return nil();
    else if (is_eq(var, car(vars))) return car(vals);
    else return find(var, cdr(vars), cdr(vals)); 
}

static int
add_var_if_consistent(const TYPE* var, 
                      const TYPE* val, 
                      TYPE** vars, TYPE** vals)
{
    int result = FALSE;
    TYPE* old_val = find(var, *vars, *vals);

    if (IS_NIL(old_val))
    {
        result = TRUE;
        *vars = cons(var, *vars);
        *vals = cons(val, *vals);
    }
    else if (is_eq(val, old_val))
    {
        result = TRUE;
    }
    
    return result;
}

static int
is_pattern_var(const TYPE* var)
{
    return (is_symbol(var) && (var->d.s[0] == '?'));
}

static TYPE*
name_from_pattern_var(const TYPE* var)
{
    const char* s = var->d.s;
    s++;
    return mk_symbol(s);
}

static int
is_wildcard_pattern(const TYPE* pattern)
{
    return strlen(pattern->d.s) == 1 && pattern->d.s[0] == '?'; 
}

static int
pattern_match(const TYPE* data, const TYPE* pattern, TYPE** vars, TYPE** vals)
{
    int result = FALSE;
	
    if (is_pattern_var(pattern))
    {
        if (is_wildcard_pattern(pattern))
        {
            result = TRUE;
        }
        else
        {
            result = add_var_if_consistent(name_from_pattern_var(pattern),
                                           data,
                                           vars,
                                           vals);
        }            
    }
    else if (is_pair(data) && is_pair(pattern))
    {
        result = 
            pattern_match(car(data), car(pattern), vars, vals) &&
            pattern_match(cdr(data), cdr(pattern), vars, vals);
    }
    else if (is_eq(data, pattern))
    {
        result = TRUE;
    }

    return result;
}

/* sets reg_exp to the expresision to evaluate if match is found */
/* reg_val contains the match key and reg_unev contains the clauses */
/* vars and vals contains the variable bindings from the match */
static int 
find_matching_match_clause(const TYPE* key, 
                           const TYPE* clauses, 
                           TYPE** vars, 
                           TYPE** vals, 
                           TYPE** eval_exp)
{
    int result = FALSE;
    TYPE* first_clause;
    TYPE* tmp_vars = nil();
    TYPE* tmp_vals = nil();

    if (!IS_NIL(clauses))
    {
        first_clause = car(clauses);
        if (pattern_match(key, car(first_clause), &tmp_vars, &tmp_vals))
        { 
            *eval_exp = mk_begin(cdr(first_clause));
            *vars = tmp_vars;
            *vals = tmp_vals;
            result = TRUE;
        }
        else
        {
            result = find_matching_match_clause(key, 
                                                cdr(clauses), 
                                                vars, 
                                                vals,
                                                eval_exp);
        }
    }

    return result;
}

static int 
is_apply(const TYPE* exp)
{
    return is_tagged_list(exp, _apply_keyword_symbol_);
}

static TYPE*
apply_arguments(const TYPE* exp)
{
	return cdr(cdr(exp));
}

static TYPE*
apply_procedure(const TYPE* exp)
{
    return car(cdr(exp));
}

static int 
is_delay(const TYPE* exp)
{
    return is_tagged_list(exp, _delay_keyword_symbol_);
}

static int 
is_stream_cons(const TYPE* exp)
{
    return is_tagged_list(exp, _stream_cons_keyword_symbol_);
}


static int
is_improper_list(const TYPE* exp)
{
    if (IS_NIL(exp))
    {
        return FALSE;
    } 
    else if (is_pair(exp))
    {
        return is_improper_list(cdr(exp));
    } 
    else 
    {
        return TRUE;
    } 
}

static int
improper_list_length(const TYPE* exp)
{
    if (is_pair(exp)) 
    {
        return 1 + improper_list_length(cdr(exp));
    } 
    else
    {
        return 0;
    }
}

static void
check_procedure_arg_len(const TYPE* proc_name, int arg_len, const TYPE* params)
{
    if (is_improper_list(params))
    {
		int min_len = improper_list_length(params);
		if (arg_len < min_len) {
			display_debug(proc_name);
			fprintf(stderr, "CHECK_PROCEDURE_ARG_LEN: got %d args expected at least %d args\n",
					arg_len,
					min_len);
			throw_error(APPLY_ERROR, 
						"Apply: wrong number of arguments in application");
		}
    }
    else
    {
        int len = length(params);
		if (arg_len != len) {
			display_debug(proc_name);
			fprintf(stderr, "CHECK_PROCEDURE_ARG_LEN: got %d args expected %d args\n",
					arg_len,
					len);
			throw_error(APPLY_ERROR, 
						"Apply: wrong number of arguments in application");
		}
    }
}

static int
is_call_cc(const TYPE* exp)
{
	return is_tagged_list(exp, _call_cc_keyword_symbol_);
}

void
hairy_eval()
{
    TYPE* vars;                 /* used in match */
    TYPE* vals;                 /* used in match */
	TYPE* tmp;
	
    reg.val = nil();
    reg.cont = &&done;
    reg.proc = nil();
    reg.arg1 = nil();
    reg.unev = nil();

eval_dispatch:
    if (_debug_)
    {
        fprintf(stderr, "eval_dispatch\n");
        display_debug(reg.exp);
        fflush(NULL);
    }
    
    if (is_self_evaluating(reg.exp)){goto ev_self_eval;}
    else if (is_variable(reg.exp)){goto ev_variable;}
    else if (is_quoted(reg.exp)){goto ev_quoted;}
    else if (is_assignment(reg.exp)){goto ev_assignment;}
    else if (is_definition(reg.exp)){goto ev_definition;}
    else if (is_if(reg.exp)){goto ev_if;}
    else if (is_lambda(reg.exp)){goto ev_lambda;}
    else if (is_begin(reg.exp)){goto ev_begin;}
    else if (is_match(reg.exp)){goto ev_match;}
    else if (is_delay(reg.exp)){goto ev_delay;}
    else if (is_stream_cons(reg.exp)){goto ev_stream_cons;}
	else if (is_call_cc(reg.exp)){goto ev_call_cc;}
    else if (is_apply(reg.exp)){goto ev_apply;}
    else if (is_application(reg.exp)){goto ev_application;}
    else
    {
        display_debug(reg.exp);
        throw_error(APPLY_ERROR, "EVAL: not a valid expression");
    }
ev_self_eval:
    reg.val = reg.exp;
    goto *reg.cont;
ev_variable:
    reg.val = lookup_variable_value(reg.exp, reg.env);
    goto *reg.cont;
ev_quoted:
    reg.val = quotation_value(reg.exp);
    goto *reg.cont;
ev_lambda:
    reg.val = mk_procedure(lambda_parameters(reg.exp),
                           lambda_body(reg.exp),
                           reg.env);
    goto *reg.cont;
ev_begin:
    reg.unev = begin_actions(reg.exp);
    save(reg.cont);
    goto ev_sequence;
ev_match:
    save(reg.exp);
    save(reg.cont);
    if (length(reg.exp) < 2) throw_error(EVAL_ERROR, "MATCH: key is missing.");
    reg.unev = match_clauses(reg.exp);
    save(reg.unev);
    reg.exp = match_key(reg.exp);
    reg.cont = &&ev_match_key_evaluated;
    goto eval_dispatch;
ev_match_key_evaluated:
    restore(&reg.unev);
    save(reg.env);
    /* sets reg.exp to the expresion to evaluate if match is found */
    /* reg.val contains the match key and reg.unev contains the clauses */
    /* vars and vals contains the variable bindings from the match */
    if (find_matching_match_clause(reg.val, 
                                   reg.unev, 
                                   &vars, 
                                   &vals, 
                                   (TYPE**)&reg.exp))
    {
        if (length(vars) > 0)
        {
            reg.env = extend_environment(reverse(vars), reverse(vals), reg.env);
        }
        reg.cont = &&ev_match_done;
        goto eval_dispatch;
    }
    /* no match, return value none */
    reg.val = mk_none();
ev_match_done:
    restore(&reg.env);
    restore(&reg.cont);
    restore(&reg.exp);
    goto *reg.cont;
ev_delay:
	reg.val = mk_procedure(_nil_, operands(reg.exp), reg.env);
    goto *reg.cont;
ev_stream_cons:
    save(reg.unev);
    reg.unev = operands(reg.exp);
    if (length(reg.unev) != 2)
    {
        throw_error(EVAL_ERROR, 
                    "STREAM_CONS: must have 2 operands");
    }
    reg.exp = cons(mk_symbol("delay"), cdr(reg.unev));
    save(reg.cont);
    reg.cont = &&ev_stream_cons_done; 
    goto eval_dispatch;
ev_stream_cons_done:
    reg.val = cons(car(reg.unev), reg.val);
    restore(&reg.cont);
    restore(&reg.unev);
    goto *reg.cont;
ev_call_cc:
	save(reg.cont);
    if (length(reg.exp) != 2)
    {
        throw_error(EVAL_ERROR, "CALL_CC: must have exactly one operand");
    }
    reg.exp = mk_list(2, car(operands(reg.exp)), mk_escape_proc(get_stack(), &reg));
    reg.cont = &&ev_call_cc_done;
    goto eval_dispatch;
ev_call_cc_done:
	restore(&reg.cont);
    goto *reg.cont;
    /* special form apply */
ev_apply:
    reg.unev = apply_arguments(reg.exp);
	if (IS_NIL(reg.unev)) {
		throw_error(APPLY_ERROR,
                    "APPLY: needs at least one argument");
	}
    if (!is_pair(reg.unev))
    {
		display_debug(reg.unev);
		throw_error(APPLY_ERROR,
                    "APPLY: malformed arguments in application");
    }
	reg.exp = apply_procedure(reg.exp);
	save(reg.cont);
	save(reg.exp);
	save(reg.env);
    save(reg.unev);
    reg.cont = &&ev_apply_did_operator;
    goto eval_dispatch;
ev_apply_did_operator:
    restore(&reg.unev);
    restore(&reg.env);
    reg.arg1 = nil();
    reg.proc = reg.val;
	if (no_operands(reg.unev)) goto apply_dispatch;
    save (reg.proc);
ev_apply_operand_loop:
    save(reg.arg1);
    reg.exp = first_operand(reg.unev);
    if (last_operand(reg.unev)) goto ev_apply_last_arg;
    save(reg.env);
    save(reg.unev);
    reg.cont = &&ev_apply_accumulate_arg;
    goto eval_dispatch;
ev_apply_accumulate_arg:
    restore(&reg.unev);
    restore(&reg.env);
    restore(&reg.arg1);
    reg.arg1 = cons(reg.val, reg.arg1);
    reg.unev = rest_operands(reg.unev);
    goto ev_apply_operand_loop;
ev_apply_last_arg:
    reg.cont = &&ev_apply_accum_last_arg;
    goto eval_dispatch;
ev_apply_accum_last_arg:
    restore(&reg.arg1);
    if (!is_list(reg.val))
    {
        throw_error(APPLY_ERROR,
                    "APPLY: last argument must be a list");
    }
    reg.arg1 = append(reverse(reg.arg1), reg.val);
    restore(&reg.proc);
    goto apply_dispatch;
    /* application of operator to operands */
ev_application:
	reg.unev = operands(reg.exp);
    if (!IS_NIL(reg.unev) && !is_pair(reg.unev))
    {
		display_debug(reg.unev);
		throw_error(APPLY_ERROR,
                    "APPLICATION: malformed arguments in application");
    }
	reg.exp = operator(reg.exp);
	save(reg.cont);
	save(reg.exp);
	save(reg.env);
    save(reg.unev);
    reg.cont = &&ev_appl_did_operator;
    goto eval_dispatch;
ev_appl_did_operator:
    restore(&reg.unev);
    restore(&reg.env);
    reg.arg1 = nil();
    reg.proc = reg.val;
    if (no_operands(reg.unev)) goto apply_dispatch;
    save (reg.proc);
ev_appl_operand_loop:
    save(reg.arg1);
    reg.exp = first_operand(reg.unev);
    if (last_operand(reg.unev)) goto ev_appl_last_arg;
    save(reg.env);
    save(reg.unev);
    reg.cont = &&ev_appl_accumulate_arg;
    goto eval_dispatch;
ev_appl_accumulate_arg:
    restore(&reg.unev);
    restore(&reg.env);
    restore(&reg.arg1);
    reg.arg1 = cons(reg.val, reg.arg1);
    reg.unev = rest_operands(reg.unev);
    goto ev_appl_operand_loop;
ev_appl_last_arg:
    reg.cont = &&ev_appl_accum_last_arg;
    goto eval_dispatch;
ev_appl_accum_last_arg:
    restore(&reg.arg1);
    reg.arg1 = reverse(cons(reg.val, reg.arg1));
    restore(&reg.proc);
    goto apply_dispatch;
ev_sequence:
    reg.exp = first_exp(reg.unev);
    if (is_last_exp(reg.unev)) goto ev_sequence_last_exp;
    save(reg.unev);
    save(reg.env);
    reg.cont = &&ev_sequence_continue;
    goto eval_dispatch;
ev_sequence_continue:
    restore(&reg.env);
    restore(&reg.unev);
    reg.unev = rest_exps(reg.unev);
    goto ev_sequence;
ev_sequence_last_exp:
    restore(&reg.cont);
    goto eval_dispatch;
ev_if:
    save(reg.exp);
    save(reg.env);
    save(reg.cont);
    reg.cont = &&ev_if_decide;
    reg.exp = if_predicate(reg.exp);
    goto eval_dispatch;
ev_if_decide:
    restore(&reg.cont);
    restore(&reg.env);
    restore(&reg.exp);
    if (is_true(reg.val)) goto ev_if_consequent;
ev_if_alternative:
    reg.exp = if_alternative(reg.exp);
    goto eval_dispatch;
ev_if_consequent:
    reg.exp = if_consequent(reg.exp);
    goto eval_dispatch;
ev_assignment:
    reg.unev = assignment_variable(reg.exp);
    save(reg.unev);
    reg.exp = assignment_value(reg.exp);
    save(reg.env);
    save(reg.cont);
    reg.cont = &&ev_assignment_1;
    goto eval_dispatch;
ev_assignment_1:
    restore(&reg.cont);
    restore(&reg.env);
    restore(&reg.unev);
    set_variable_value(reg.unev, reg.val, reg.env);
    reg.val = mk_none();
    goto *reg.cont;
ev_definition:
    reg.unev = definition_variable(reg.exp);
    save(reg.unev);
    reg.exp = definition_value(reg.exp);
    save(reg.env);
    save(reg.cont);
    reg.cont = &&ev_definition_1;
    goto eval_dispatch;
ev_definition_1:
    restore(&reg.cont);
    restore(&reg.env);
    restore(&reg.unev);
    define_variable(reg.unev, reg.val, reg.env);
    goto *reg.cont;
    /* Apply */
apply_dispatch:
	restore(&reg.exp); // the name of the procedure for error reporting
    if (is_primitive_procedure(reg.proc)) goto primitive_apply;
    else if (is_compound_procedure(reg.proc)) goto compound_apply;
	else if (is_escape_proc(reg.proc)) goto escape_proc_apply;
    else
	{
		display_debug(reg.exp);
		display_debug(reg.proc);
		throw_error(APPLY_ERROR, "Apply: not a valid procedure type");
	}
primitive_apply:
    reg.val = apply_primitive_procedure(reg.proc, reg.arg1, reg.env);
    restore(&reg.cont);
    goto *reg.cont;
compound_apply:
	tmp = procedure_parameters(reg.proc);
	check_procedure_arg_len(reg.exp, length(reg.arg1), tmp);
    reg.env = extend_environment(tmp,
                                 reg.arg1,
                                 procedure_environment(reg.proc));
    reg.unev = procedure_body(reg.proc);
    goto ev_sequence;
escape_proc_apply:
	if (length(reg.arg1) != 1)
    {
        display_debug(reg.exp);
        fprintf(stderr, "escape procedure got %d args expected one argument\n",
                length(reg.arg1));
        throw_error(APPLY_ERROR, 
                    "Apply: wrong number of arguments in escape procedure application");
    }
	tmp = car(reg.arg1);
	assign_stack(&(((TYPE*)reg.proc)->d.e->stack));
	copy_regs(&reg, &(((TYPE*)reg.proc)->d.e->regs));
	reg.val = tmp;
	restore(&reg.cont);
    goto *reg.cont;
done:
    ;
}

/* translation to name free lambda expressions */
static 
TYPE* 
extend_context(const TYPE* vars, const TYPE* context)
{
    return cons(vars, context);
}

static int 
symbol_exist_in_improper_list(const TYPE* symbol, const TYPE* list)
{
    int result;

    if (IS_NIL(list))
    {
        result = FALSE;
    }
    else if (is_symbol(list))
    {
        if (is_eq(symbol, list))
        {
            result = TRUE;
        }
        else
        {
            result = FALSE;
        }
    }
    else if (is_eq(symbol, car(list)))
    {
        result = TRUE;
    }
    else 
    {
        result = symbol_exist_in_improper_list(symbol, cdr(list));
    }
    
    return result;
}

static
void
add_definition_to_context(const TYPE* var, TYPE* context)
{
    if (!IS_NIL(context) && !symbol_exist_in_improper_list(var, context)) 
    {
        set_car(context, cons(var, car(context)));
    }
}

static
int
get_var_index(const TYPE* var, 
              const TYPE* vars, 
              int index, 
              int* is_inproper_list)
{
    int result;

    if (IS_NIL(vars))
    {
        result= -1;
    } 
    else if (is_symbol(vars)) /* var # args case */
    {
        if (is_eq(var, vars)) 
        {
            result = index;
            *is_inproper_list = TRUE;
        }
        else
        {
            result = -1;
        }
    } 
    else if (is_eq(var, car(vars)))
    {
        result = index;
        *is_inproper_list = FALSE;
    } 
    else 
    {
        result = get_var_index(var, cdr(vars), ++index, is_inproper_list);
    }

    return result;
}

static 
TYPE* 
symbol_to_bound_var(TYPE* var, TYPE* context, unsigned int frame_index)
{
    TYPE* result;

    if (IS_NIL(context)) 
    {
        result = var;
    } 
    else
    {
        int is_inproper_list;
        int var_index = get_var_index(var, car(context), 0, &is_inproper_list);
        
        if (var_index == -1)
        {
            result = symbol_to_bound_var(var, cdr(context), ++frame_index);
        } 
        else 
        {
            result = mk_bound_var(var, 
                                  frame_index, 
                                  var_index, 
                                  is_inproper_list);
        }
    }
    
    return result;
}

static
TYPE*
get_vars_from_match(TYPE* pattern)
{
    TYPE* result = nil();

    if (is_symbol(pattern))
    {
        if (is_pattern_var(pattern) && !is_wildcard_pattern(pattern))
        {
            result = cons(name_from_pattern_var(pattern), result);
        }
    }
    else if (is_pair(pattern))
    {
        TYPE* first = car(pattern);
        
        if (is_pattern_var(first) && !is_wildcard_pattern(first))
        {
            result = cons(name_from_pattern_var(first), 
                          get_vars_from_match(cdr(pattern)));
        }
        else if (is_pair(first))
        {
            TYPE* vars = get_vars_from_match(first);

            if (IS_NIL(vars)) 
            {
                result = get_vars_from_match(cdr(pattern));
            } 
            else
            {
                result = append(vars, get_vars_from_match(cdr(pattern)));
            }
        }
        else
        {
            result = get_vars_from_match(cdr(pattern));
        }
    }
    
    return result;
}

static TYPE* exp_to_name_free_exp(TYPE* exp, TYPE* context);

static
TYPE*
match_clauses_to_name_free(TYPE* clauses, 
                           TYPE* context)
{
    TYPE* result;

    if (IS_NIL(clauses))
    {
        result = clauses;
    } 
    else
    {
        TYPE* first = car(clauses);
        TYPE* pattern = car(first);
        TYPE* body = cdr(first);
        TYPE* vars = get_vars_from_match(pattern);
        TYPE* match_body_context;

        if (IS_NIL(vars))
        {
            match_body_context = context;
        }
        else
        {
            match_body_context = extend_context(vars, context);
        }

        result = cons(cons(pattern,
                           exp_to_name_free_exp(body,
                                                match_body_context)),
                      match_clauses_to_name_free(cdr(clauses),
												 context));
    }
    
    return result;
}

static 
TYPE*
scan_internal_defs(TYPE* exps)
{
	TYPE* result = nil();
	int found_non_definition = 0;
	while (!IS_NIL(exps))
	{
		TYPE* exp = car(exps);
		if (is_definition(exp))
		{
			if (found_non_definition)
			{
				throw_error(PARSE_ERROR, 
							"EVAL: internal definition must come first in a body");
			}
			TYPE* var = definition_variable(exp);
			result = cons(var, result);
		} else {
			found_non_definition = 1;
		}
		exps = cdr(exps);
	}

	return reverse(result);
}

static
void
add_defs_to_context(TYPE* defs, TYPE* ctx)
{
	while (!IS_NIL(defs))
	{
		add_definition_to_context(car(defs), ctx);
		defs = cdr(defs);
	}
}

static 
TYPE*
exp_to_name_free_exp(TYPE* exp, TYPE* context)
{
    TYPE* result;
    
    if (IS_NIL(exp))
    {
        result = exp;
    } 
    else if (is_quoted(exp))
    {
        result = exp;
    }
    else if (is_lambda(exp))
    {
        TYPE* vars = lambda_parameters(exp);
		TYPE* ctx = extend_context(vars, context);
		TYPE* body = lambda_body(exp);
		TYPE* defs = scan_internal_defs(body);
		add_defs_to_context(defs, ctx);
        result = mk_lambda(vars, 
                           exp_to_name_free_exp(body,
												ctx));
    } 
    else if (is_definition(exp))
    {
        TYPE* var = definition_variable(exp);
		TYPE* val = definition_value(exp);
        result = mk_definition(var,
                               exp_to_name_free_exp(val, context));
    }
    else if (is_match(exp))
    {
        TYPE* key = match_key(exp);
        TYPE* clauses = match_clauses(exp);

        result = mk_match(exp_to_name_free_exp(key, context), 
                          match_clauses_to_name_free(clauses, context));
    }
	else if (is_delay(exp)) {
		assert_throw(length(exp) == 2,
					 PARSE_ERROR,
					 "DELAY: must have exactly one argument");
		TYPE* ctx = extend_context(nil(), context);
		result = mk_list(2, _delay_keyword_symbol_, exp_to_name_free_exp(cadr(exp), ctx));
	}
    else if (is_pair(exp))
    {
        TYPE* hd = exp_to_name_free_exp(car(exp), context);
        TYPE* tl = exp_to_name_free_exp(cdr(exp), context);
        result = cons(hd, tl);
    } 
    else if (is_symbol(exp))
    {
        result = symbol_to_bound_var(exp, context, 0);
    }
    else
    {
        result = exp;
    }

    return result;
}

TYPE*
eval(TYPE* exp, TYPE* env)
{
    /* reg.exp = cons(mk_symbol("__internal-translate__"), */
    /*                cons(mk_quoted(exp), nil())); */
    /* reg.env = env; */
    /* hairy_eval(); */
    /* eval_no_translation(reg.val, env); */
    eval_no_translation(exp, env);

    return reg.val;
}

TYPE*
data_eval(TYPE* exp, TYPE* env)
{
    return eval(xlat(exp), env);
}

TYPE*
eval_no_translation(TYPE* exp, TYPE* env)
{
    reg.exp = exp_to_name_free_exp(exp, nil());
    reg.env = env;
    hairy_eval();
	if (_debug_) {
		stack_print_statistics();
	}
    return reg.val;
}
