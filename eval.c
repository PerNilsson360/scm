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

extern int _debug_;             /* defined in primitive_procedure */

/* registers for scheme VM */

static void* reg_exp;
static void* reg_env;
static void* reg_val;
static void* reg_cont;
static void* reg_proc;
static void* reg_arg1;
static void* reg_unev;

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
        is_nil(sexp);
}

static
inline
int
is_variable(const TYPE* exp)
{
    return is_symbol(exp) || is_bound_var(exp);
}

int
is_tagged_list(const TYPE* exp, TYPE* symbol)
{
    return is_pair(exp) && is_eq(car(exp), symbol);
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

TYPE*
mk_lambda(TYPE* parameters, TYPE* body)
{
    return cons(_lambda_keyword_symbol_, cons(parameters, body));
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
    
    if (!is_nil(cdr(cdr(cdr(exp)))))
    {
        result = car(cdr(cdr(cdr(exp))));
    }

    return result;
}

TYPE*
mk_if(TYPE* predicate, TYPE* consequent, TYPE* alternative)
{
    return cons(_if_keyword_symbol_,
                cons(predicate,
                     cons(consequent, 
                          cons(alternative, nil()))));
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

int
is_last_exp(TYPE* exps)
{
    return is_nil(cdr(exps));
}

TYPE* 
first_exp(TYPE* exps)
{
    return car(exps);
}

TYPE* 
rest_exps(TYPE* exps)
{
    return cdr(exps);
}

TYPE*
mk_begin(TYPE* exp)
{
    return cons(_begin_keyword_symbol_, exp);
}

TYPE*
sequence_to_exp(TYPE* seq)
{
    TYPE* result;

    if (is_nil(seq))
    {
        result = nil();
    }
    else if (is_last_exp(seq))
    {
        result = first_exp(seq);
    }
    else
    {
        result = mk_begin(seq);
    }

    return result;
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

TYPE*
operands(TYPE* exp)
{
    return cdr(exp);
}

TYPE*
first_operand(TYPE* operands)
{
    return car(operands);
}

int
last_operand(TYPE* operands)
{
    return is_nil(cdr(operands));
}

int 
no_operands(TYPE* operands)
{
    return is_nil(operands);
}

TYPE*
rest_operands(TYPE* operands)
{
    return cdr(operands);
}

int
is_match(const TYPE* exp)
{
    return is_tagged_list(exp, _match_keyword_symbol_);
}

TYPE*
mk_match(TYPE* key, TYPE* clauses)
{
    return cons(_match_keyword_symbol_, cons(key, clauses));
}


TYPE*
match_key(TYPE* exp)
{
    return car(cdr(exp));
}

TYPE*
match_clauses(TYPE* exp)
{
    return cdr(cdr(exp));
}

static TYPE*
find(const TYPE* var, TYPE* vars, TYPE* vals)
{
    if (is_nil(vars)) return nil();
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

    if (is_nil(old_val))
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

    if (!is_nil(clauses))
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

int 
is_apply(const TYPE* exp)
{
    const char* apply = "apply";   
    return is_tagged_list(exp, mk_symbol(apply));
}

TYPE*
apply_arguments(const TYPE* exp)
{
    return cdr(cdr(exp));
}

TYPE*
apply_procedure(const TYPE* exp)
{
    return car(cdr(exp));
}

int 
is_delay(const TYPE* exp)
{
    return is_tagged_list(exp, _delay_keyword_symbol_);
}

int 
is_stream_cons(const TYPE* exp)
{
    return is_tagged_list(exp, _stream_cons_keyword_symbol_);
}


int
is_improper_list(const TYPE* exp)
{
    if (is_nil(exp))
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

int
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

int 
min_procedure_parameters(const TYPE* exp)
{
    if (is_improper_list(exp))
    {
        return improper_list_length(exp);
    }
    else
    {
        return length(exp);
    }
}

void
hairy_eval()
{
    TYPE* vars;                 /* used in match */
    TYPE* vals;                 /* used in match */

    TYPE* reg_exp_debug = nil();
    reg_val = nil();
    reg_cont = &&done;
    reg_proc = nil();
    reg_arg1 = nil();
    reg_unev = nil();

eval_dispatch:
    if (_debug_)
    {
        fprintf(stderr, "eval_dispatch\n");
        display_debug(reg_exp);
        fflush(NULL);
    }
    
    if (is_self_evaluating(reg_exp)){goto ev_self_eval;}
    else if (is_variable(reg_exp)){goto ev_variable;}
    else if (is_quoted(reg_exp)){goto ev_quoted;}
    else if (is_assignment(reg_exp)){goto ev_assignment;}
    else if (is_definition(reg_exp)){goto ev_definition;}
    else if (is_if(reg_exp)){goto ev_if;}
    else if (is_lambda(reg_exp)){goto ev_lambda;}
    else if (is_begin(reg_exp)){goto ev_begin;}
    else if (is_match(reg_exp)){goto ev_match;}
    else if (is_delay(reg_exp)){goto ev_delay;}
    else if (is_stream_cons(reg_exp)){goto ev_stream_cons;}
    else if (is_apply(reg_exp)){goto ev_apply;}
    else if (is_application(reg_exp)){goto ev_application;}
    else
    {
        display_debug(reg_exp);
        throw_error(APPLY_ERROR, "EVAL: not a valid expression");
    }
ev_self_eval:
    reg_val = reg_exp;
    goto *reg_cont;
ev_variable:
    reg_val = lookup_variable_value(reg_exp, reg_env);
    goto *reg_cont;
ev_quoted:
    reg_val = quotation_value(reg_exp);
    goto *reg_cont;
ev_lambda:
    reg_val = mk_procedure(lambda_parameters(reg_exp),
                           lambda_body(reg_exp),
                           reg_env);
    goto *reg_cont;
ev_begin:
    reg_unev = begin_actions(reg_exp);
    save(reg_cont);
    goto ev_sequence;
ev_match:
    save(reg_exp);
    save(reg_cont);
    if (length(reg_exp) < 2) throw_error(EVAL_ERROR, "MATCH: key is missing.");
    reg_unev = match_clauses(reg_exp);
    save(reg_unev);
    reg_exp = match_key(reg_exp);
    reg_cont = &&ev_match_key_evaluated;
    goto eval_dispatch;
ev_match_key_evaluated:
    restore(&reg_unev);
    save(reg_env);
    /* sets reg_exp to the expresion to evaluate if match is found */
    /* reg_val contains the match key and reg_unev contains the clauses */
    /* vars and vals contains the variable bindings from the match */
    if (find_matching_match_clause(reg_val, 
                                   reg_unev, 
                                   &vars, 
                                   &vals, 
                                   (TYPE**)&reg_exp))
    {
        if (length(vars) > 0)
        {
            reg_env = extend_environment(reverse(vars), reverse(vals), reg_env);
        }
        reg_cont = &&ev_match_done;
        goto eval_dispatch;
    }
    /* no match, return value none */
    reg_val = mk_none();
ev_match_done:
    restore(&reg_env);
    restore(&reg_cont);
    restore(&reg_exp);
    goto *reg_cont;
ev_delay:
    save(reg_cont);
    if (length(reg_exp) != 2)
    {
        throw_error(EVAL_ERROR, "DELAY: must have exactly one operand");
    }
    reg_exp = mk_lambda(nil(), operands(reg_exp));
    reg_cont = &&ev_delay_done;
    goto eval_dispatch;
ev_delay_done:
    restore(&reg_cont);
    goto *reg_cont;
ev_stream_cons:
    save(reg_unev);
    reg_unev = operands(reg_exp);
    if (length(reg_unev) != 2)
    {
        throw_error(EVAL_ERROR, 
                    "STREAM_CONS: must have 2 operands");
    }
    reg_exp = cons(mk_symbol("delay"), cdr(reg_unev));
    save(reg_cont);
    reg_cont = &&ev_stream_cons_done; 
    goto eval_dispatch;
ev_stream_cons_done:
    reg_val = cons(car(reg_unev), reg_val);
    restore(&reg_cont);
    restore(&reg_unev);
    goto *reg_cont;
    /* special form apply */
ev_apply:
    /* @todo the aggregation of operands are not according to r5rs */
    save(reg_cont);
    save(reg_env);
    reg_unev = apply_arguments(reg_exp);
    if (!is_nil(reg_unev) && !is_pair(reg_unev))
    {
	display_debug(reg_unev);
	throw_error(APPLY_ERROR,
                    "APPLY: malformed arguments in application");
    }
    save(reg_unev);
    reg_exp = apply_procedure(reg_exp);
    reg_exp_debug = reg_exp;	/* need to know when rands are wrong */
    reg_cont = &&ev_apply_did_operator;
    goto eval_dispatch;
ev_apply_did_operator:
    restore(&reg_unev);
    restore(&reg_env);
    reg_arg1 = nil();
    reg_proc = reg_val;    
    if (no_operands(reg_unev)) 
    {
        throw_error(APPLY_ERROR,
                    "APPLY: wrong number of arguments");
    }
    save (reg_proc);
ev_apply_operand_loop:
    save(reg_arg1);
    reg_exp = first_operand(reg_unev);
    if (last_operand(reg_unev)) goto ev_apply_last_arg;
    save(reg_env);
    save(reg_unev);
    reg_cont = &&ev_apply_accumulate_arg;
    goto eval_dispatch;
ev_apply_accumulate_arg:
    restore(&reg_unev);
    restore(&reg_env);
    restore(&reg_arg1);
    reg_arg1 = cons(reg_val, reg_arg1);
    reg_unev = rest_operands(reg_unev);
    goto ev_apply_operand_loop;
ev_apply_last_arg:
    reg_cont = &&ev_apply_accum_last_arg;
    goto eval_dispatch;
ev_apply_accum_last_arg:
    restore(&reg_arg1);
    if (!is_list(reg_val))
    {
        throw_error(APPLY_ERROR,
                    "APPLY: last argument must be a list");
    }
    reg_arg1 = append(reverse(reg_arg1), reg_val);
    restore(&reg_proc);
    goto apply_dispatch;
    /* application of operator to operands */
ev_application:
    save(reg_cont);
    save(reg_env);
    reg_unev = operands(reg_exp);
    if (!is_nil(reg_unev) && !is_pair(reg_unev))
    {
	display_debug(reg_unev);
	throw_error(APPLY_ERROR,
                    "APPLICATION: malformed arguments in application");
    }

    save(reg_unev);
    reg_exp = operator(reg_exp);
    reg_cont = &&ev_appl_did_operator;
    goto eval_dispatch;
ev_appl_did_operator:
    restore(&reg_unev);
    restore(&reg_env);
    reg_arg1 = nil();
    reg_proc = reg_val;    
    if (no_operands(reg_unev)) goto apply_dispatch;
    save (reg_proc);
ev_appl_operand_loop:
    save(reg_arg1);
    reg_exp = first_operand(reg_unev);
    if (last_operand(reg_unev)) goto ev_appl_last_arg;
    save(reg_env);
    save(reg_unev);
    reg_cont = &&ev_appl_accumulate_arg;
    goto eval_dispatch;
ev_appl_accumulate_arg:
    restore(&reg_unev);
    restore(&reg_env);
    restore(&reg_arg1);
    reg_arg1 = cons(reg_val, reg_arg1);
    reg_unev = rest_operands(reg_unev);
    goto ev_appl_operand_loop;
ev_appl_last_arg:
    reg_cont = &&ev_appl_accum_last_arg;
    goto eval_dispatch;
ev_appl_accum_last_arg:
    restore(&reg_arg1);
    reg_arg1 = reverse(cons(reg_val, reg_arg1));
    restore(&reg_proc);
    goto apply_dispatch;
ev_sequence:
    reg_exp = first_exp(reg_unev);
    if (is_last_exp(reg_unev)) goto ev_sequence_last_exp;
    save(reg_unev);
    save(reg_env);
    reg_cont = &&ev_sequence_continue;
    goto eval_dispatch;
ev_sequence_continue:
    restore(&reg_env);
    restore(&reg_unev);
    reg_unev = rest_exps(reg_unev);
    goto ev_sequence;
ev_sequence_last_exp:
    restore(&reg_cont);
    goto eval_dispatch;
ev_if:
    save(reg_exp);
    save(reg_env);
    save(reg_cont);
    reg_cont = &&ev_if_decide;
    reg_exp = if_predicate(reg_exp);
    goto eval_dispatch;
ev_if_decide:
    restore(&reg_cont);
    restore(&reg_env);
    restore(&reg_exp);
    if (is_true(reg_val)) goto ev_if_consequent;
ev_if_alternative:
    reg_exp = if_alternative(reg_exp);
    goto eval_dispatch;
ev_if_consequent:
    reg_exp = if_consequent(reg_exp);
    goto eval_dispatch;
ev_assignment:
    reg_unev = assignment_variable(reg_exp);
    save(reg_unev);
    reg_exp = assignment_value(reg_exp);
    save(reg_env);
    save(reg_cont);
    reg_cont = &&ev_assignment_1;
    goto eval_dispatch;
ev_assignment_1:
    restore(&reg_cont);
    restore(&reg_env);
    restore(&reg_unev);
    set_variable_value(reg_unev, reg_val, reg_env);
    reg_val = mk_none();
    goto *reg_cont;
ev_definition:
    reg_unev = definition_variable(reg_exp);
    save(reg_unev);
    reg_exp = definition_value(reg_exp);
    save(reg_env);
    save(reg_cont);
    reg_cont = &&ev_definition_1;
    goto eval_dispatch;
ev_definition_1:
    restore(&reg_cont);
    restore(&reg_env);
    restore(&reg_unev);
    define_variable(reg_unev, reg_val, reg_env);
    goto *reg_cont;
    /* Apply */
apply_dispatch:
    if (is_primitive_procedure(reg_proc)) goto primitive_apply;
    else if (is_compound_procedure(reg_proc)) goto compound_apply;
    else throw_error(APPLY_ERROR, "Apply: not a valid procedure type");
primitive_apply:
    reg_val = apply_primitive_procedure(reg_proc, reg_arg1, reg_env);
    restore(&reg_cont);
    goto *reg_cont;
compound_apply:
    if (length(reg_arg1) < 
        min_procedure_parameters(procedure_parameters(reg_proc)))
    {
        display_debug(reg_exp_debug);
        fprintf(stderr, "got %d args expected at least %d args\n",
                length(reg_arg1),
                min_procedure_parameters(procedure_parameters(reg_proc)));
        throw_error(APPLY_ERROR, 
                    "Apply: wrong number of arguments in application");
    }
    reg_env = extend_environment(procedure_parameters(reg_proc),
                                 reg_arg1,
                                 procedure_environment(reg_proc));
    reg_unev = procedure_body(reg_proc);
    goto ev_sequence;
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

    if (is_nil(list))
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
    if (!is_nil(context) && !symbol_exist_in_improper_list(var, context)) 
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

    if (is_nil(vars))
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

    if (is_nil(context)) 
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

            if (is_nil(vars)) 
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

    if (is_nil(clauses))
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

        if (is_nil(vars))
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
        
  while (!is_nil(exps))
  {
      TYPE* exp = car(exps);
      if (is_definition(exp))
      {
	  TYPE* var = definition_variable(exp);
	  result = cons(var, result);
      }
      exps = cdr(exps);
  }

  return reverse(result);
}

static
void
add_defs_to_context(TYPE* defs, TYPE* ctx)
{
  while (!is_nil(defs))
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
    
    if (is_nil(exp))
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
    reg_exp = cons(mk_symbol("__internal-translate__"),
                   cons(mk_quoted(exp), nil()));
    reg_env = env;
    hairy_eval();
    eval_no_translation(reg_val, env);
    /* eval_no_translation(exp, env); */

    return reg_val;
}

TYPE*
eval_no_translation(TYPE* exp, TYPE* env)
{
    reg_exp = exp_to_name_free_exp(exp, nil());
    reg_env = env;
    hairy_eval();

    return reg_val;
}
