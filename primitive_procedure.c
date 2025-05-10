#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>

#include <gc.h>

#include "io.h"
#include "eval.h"
#include "util.h"
#include "error.h"
#include "common.h"
#include "number.h"
#include "symbol.h"
#include "char.h"
#include "str.h"
#include "blob.h"
#include "vector.h"
#include "port.h"
#include "type.h"
#include "socket.h"
#include "unix.h"
#include "graphics.h"
#include "hash_table.h"
#include "primitive_procedure.h"
#include "read.h"

int _debug_ = 0;
static TYPE* _primitive_procedure_table_;

#define MAKE_VOID_WRAPPER_NO_ARG(c_name)                                \
static                                                                  \
TYPE*                                                                   \
_ ## c_name ## _ ## procedure ## _(const TYPE* arguments, const TYPE* env)    \
{                                                                       \
    assert_throw(length(arguments) == 0,                                \
                 APPLY_ERROR,                                           \
                 "wrong # of arguments in " #c_name);                   \
    c_name();                                                           \
    return mk_none();                                                   \
}                                                                       \


#define MAKE_WRAPPER_ONE_ARG(c_name)                                    \
static                                                                  \
TYPE*                                                                   \
_ ## c_name ## _ ## procedure ## _(const TYPE* arguments, const TYPE* env)    \
{                                                                       \
    assert_throw(length(arguments) == 1,                                \
                 APPLY_ERROR,                                           \
                 "wrong # of arguments in " #c_name);                   \
    return c_name(car(arguments));                                      \
}                                                                       \

#define MAKE_VOID_WRAPPER_ONE_ARG(c_name)                                    \
static                                                                  \
TYPE*                                                                   \
_ ## c_name ## _ ## procedure ## _(const TYPE* arguments, const TYPE* env)    \
{                                                                       \
    assert_throw(length(arguments) == 1,                                \
                 APPLY_ERROR,                                           \
                 "wrong # of arguments in " #c_name);                   \
    c_name(car(arguments));                                             \
    return mk_none();                                                   \
}                                                                       \

#define MAKE_PREDICATE_WRAPPER_ONE_ARG(c_name)          \
static                                                  \
TYPE*                                                   \
_ ## c_name ## _ ## procedure ## _(const TYPE* arguments, const TYPE* env)  \
{                                                       \
    assert_throw(length(arguments) == 1,                \
                 APPLY_ERROR,                           \
                 "wrong # of arguments in " #c_name);   \
    return mk_boolean(c_name(car(arguments)));          \
}                                                       \

#define MAKE_VOID_WRAPPER_TWO_ARG(c_name)                                    \
static                                                                  \
TYPE*                                                                   \
_ ## c_name ## _ ## procedure ## _(const TYPE* arguments, const TYPE* env) \
{                                                                       \
    assert_throw(length(arguments) == 2,                                \
                 APPLY_ERROR,                                           \
                 "wrong # of arguments in " #c_name);                   \
    c_name(car(arguments), car(cdr(arguments)));			\
    return mk_none();                                                   \
}                                                                       \


#define MAKE_WRAPPER_TWO_ARGS(c_name)                   \
static                                                  \
TYPE*                                                   \
_ ## c_name ## _ ## procedure ## _(const TYPE* arguments, const TYPE* env)  \
{                                                       \
    assert_throw(length(arguments) == 2,                \
                 APPLY_ERROR,                           \
                 "wrong # of arguments in " #c_name);   \
    return c_name(car(arguments), car(cdr(arguments))); \
}                                                       \

#define MAKE_PREDICATE_WRAPPER_TWO_ARGS(c_name)         \
static                                                  \
TYPE*                                                   \
_ ## c_name ## _ ## procedure ## _(const TYPE* arguments, const TYPE* env)  \
{                                                       \
    assert_throw(length(arguments) == 2,                \
                 APPLY_ERROR,                           \
                 "wrong # of arguments in " #c_name);   \
    return mk_boolean(c_name(car(arguments), car(cdr(arguments)))); \
}                                                       \

#define MAKE_WRAPPER(c_name)						\
static								\
TYPE*                                                                   \
_ ## c_name ## _ ## procedure ## _(const TYPE* arguments, const TYPE* env) \
{                                                                       \
    return c_name(arguments);						\
}                                                                       \



#define ADD_PROCEDURE(c_name, scheme_name)                              \
    hash_table_set(_primitive_procedure_table_,                         \
                   mk_symbol(#scheme_name),                             \
                   mk_primitive_procedure(_ ## c_name ## _ ## procedure ## _))

static
TYPE* 
mk_primitive_procedure(TYPE* (f) (const TYPE* arguments, const TYPE* env))
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, 
                "MK_PRIMITIVE_PROCEDURE: could not allocate "
                "memory for type");
        exit(1);
    }

    result->d.f = mloc(sizeof(FUNCTION));

    if (result->d.f == NULL)
    {
        fprintf(stderr, "MK_PRIMITIVE_PROCEDURE: could not allocate "
                "memory for FUNCTION");
        exit(1);
    }
    
    result->type = PRIMITIVE_PROCEDURE;
    result->d.f->f = f;

    return result;
}

static 
TYPE* 
_debug_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in debug");

    _debug_ = is_true(car(arguments));

    return mk_none();
}

MAKE_WRAPPER_TWO_ARGS(data_eval);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_pair);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_procedure);
MAKE_WRAPPER_TWO_ARGS(cons);
MAKE_WRAPPER_ONE_ARG(car);
MAKE_WRAPPER_ONE_ARG(cdr);

MAKE_WRAPPER_ONE_ARG(caar); 
MAKE_WRAPPER_ONE_ARG(cadr);
MAKE_WRAPPER_ONE_ARG(caadr);
MAKE_WRAPPER_ONE_ARG(caddr);
MAKE_WRAPPER_ONE_ARG(cddr);
MAKE_WRAPPER_ONE_ARG(cdar);
MAKE_WRAPPER_ONE_ARG(cdadr);
MAKE_WRAPPER_ONE_ARG(cdddr);
MAKE_WRAPPER_ONE_ARG(cadddr);

MAKE_VOID_WRAPPER_TWO_ARG(set_car);
MAKE_VOID_WRAPPER_TWO_ARG(set_cdr);
MAKE_PREDICATE_WRAPPER_ONE_ARG(IS_NIL);
MAKE_WRAPPER(list);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_list);

static 
TYPE* 
_length_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in length");
    
    return mk_number_from_int(length(car(arguments)));
}

MAKE_WRAPPER_ONE_ARG(reverse);
MAKE_WRAPPER_ONE_ARG(unzip);

MAKE_PREDICATE_WRAPPER_ONE_ARG(is_symbol);
MAKE_WRAPPER_ONE_ARG(symbol_to_string);
MAKE_WRAPPER_ONE_ARG(string_to_symbol);
MAKE_WRAPPER_ONE_ARG(char_to_integer);
MAKE_WRAPPER_ONE_ARG(integer_to_char);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_char);
MAKE_WRAPPER_ONE_ARG(number_to_string);
MAKE_PREDICATE_WRAPPER_TWO_ARGS(is_eq);
MAKE_PREDICATE_WRAPPER_TWO_ARGS(is_eqv);
MAKE_PREDICATE_WRAPPER_TWO_ARGS(is_equal);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_number);

static 
TYPE* 
_is_complex_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in complex?");
    
    return mk_boolean(is_number(car(arguments)));
}

static 
TYPE* 
_is_real_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in real?");
    
   return mk_boolean(FALSE); /* not implemented */
}

static 
TYPE* 
_is_rational_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in rational?");

    return mk_boolean(FALSE); /* not implemented */
}

static 
TYPE* 
_is_integer_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in integer?");
    
    return mk_boolean(is_number(car(arguments))); /* only int impl */
}

static 
TYPE* 
_is_exact_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in exact?");

    assert_throw(is_number(car(arguments)),
                 APPLY_ERROR,
                 "wrong type of argument in exact?"); 

    /*  FIXME: all numbers are exact */
    return mk_boolean(TRUE); 
}

static 
TYPE* 
_is_inexact_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,         
                 "wrong # of arguments in inexact?");

    assert_throw(is_number(car(arguments)),
                 APPLY_ERROR,
                 "wrong type of argument, inexact?");

    /*  FIXME: all numbers are exact */
    return mk_boolean(FALSE); 
}

static 
TYPE* 
_equal_procedure_(const TYPE* arguments, const TYPE* env)
{
    return it_bin_pred(arguments, &is_number_equal);
}

static 
TYPE* 
_less_procedure_(const TYPE* arguments, const TYPE* env)
{
    return it_bin_pred(arguments, &is_number_lt);
}

static 
TYPE* 
_greater_procedure_(const TYPE* arguments, const TYPE* env)
{
    return it_bin_pred(arguments, &is_number_gt);
}

static 
TYPE* 
_less_equal_procedure_(const TYPE* arguments, const TYPE* env)
{
    return it_bin_pred(arguments, &is_number_lt_eq);
}

static 
TYPE* 
_greater_equal_procedure_(const TYPE* arguments, const TYPE* env)
{
    return it_bin_pred(arguments, &is_number_gt_eq);
}

MAKE_WRAPPER_ONE_ARG(is_number_zero);
MAKE_WRAPPER_ONE_ARG(is_number_positive);
MAKE_WRAPPER_ONE_ARG(is_number_negative);
MAKE_WRAPPER_ONE_ARG(is_number_odd);
MAKE_WRAPPER_ONE_ARG(is_number_even);

static 
TYPE* 
_max_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) > 0,
                 APPLY_ERROR,
                 "wrong # of arguments in max");

    return fold_right(&max_number, arguments, car(arguments));
}

static 
TYPE* 
_min_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) > 0,
                 APPLY_ERROR,
                 "wrong # of arguments in min");

    return fold_right(&min_number, arguments, car(arguments));
}

static 
TYPE* 
_plus_procedure_(const TYPE* arguments, const TYPE* env)
{
	return fold_right(&add_number, arguments, mk_number("0", 1, TRUE, 10)); 
}

static 
TYPE* 
_mul_procedure_(const TYPE* arguments, const TYPE* env)
{
  return fold_right(&mul_number, arguments, mk_number("1", 1, TRUE, 10)); 
}

static 
TYPE* 
_minus_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) > 0,
                 APPLY_ERROR,
                 "wrong # of arguments in -");

    return sub_numbers(arguments); 
}

static 
TYPE* 
_div_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) > 0,
                 APPLY_ERROR,
                 "wrong # of arguments in /");

    return div_numbers(arguments); 
}

static 
TYPE* 
_abs_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_quotient_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

MAKE_WRAPPER_TWO_ARGS(remainder_number);

static 
TYPE* 
_modulo_procedure_(const TYPE* arguments, const TYPE* env)
{
	assert(FALSE);
    return NULL;
}

static 
TYPE* 
_gcd_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_lcm_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

MAKE_WRAPPER_ONE_ARG(round_number);

static 
TYPE* 
_sin_procedure_(const TYPE* arguments, const TYPE* env)
{
	assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in sin");
	
	const TYPE* n = car(arguments);
	assert_throw(is_number(n),
                 TYPE_ERROR, 
				 "SIN: n must be a string");

	TYPE* result = mk_unasigned_number(REAL);
	result->d.d = sin(n->d.d);
	
	return result;
}

static 
TYPE* 
_cos_procedure_(const TYPE* arguments, const TYPE* env)
{
	assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in cos");
	
	const TYPE* n = car(arguments);
	assert_throw(is_number(n),
                 TYPE_ERROR, 
				 "COS: n must be a number");

	TYPE* result = mk_unasigned_number(REAL);	
	result->d.d = cos(n->d.d);
	
	return result;
}

static
TYPE*
_sqrt_procedure_(const TYPE* arguments, const TYPE* env)
{
	assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in sqrt");
	
	const TYPE* n = car(arguments);
	assert_throw(is_number(n),
                 TYPE_ERROR, 
				 "SQRT: n must be a number");

	TYPE* result = mk_unasigned_number(REAL);
	
	result->d.d = is_integer(n) ? sqrt(n->d.i) : sqrt(n->d.d);
	
	return result;
	
}

MAKE_PREDICATE_WRAPPER_ONE_ARG(is_boolean);
MAKE_WRAPPER_ONE_ARG(not);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_string);

static 
TYPE* 
_make_string_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 1 || length(arguments) == 2,
                 APPLY_ERROR,
                 "wrong # of arguments in string?");
        
    if (length(arguments) == 1)
    {
        return mk_string(car(arguments), mk_char(' '));
    }
    else
    {
        return mk_string(car(arguments), car(cdr(arguments)));
    }
}

static 
TYPE* 
_string_procedure_(const TYPE* arguments, const TYPE* env)
{
    return mk_string_from_chars(arguments);
}

MAKE_WRAPPER_ONE_ARG(string_length);
MAKE_WRAPPER_TWO_ARGS(string_ref);

static 
TYPE* 
_string_set_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 3,
                 APPLY_ERROR,
                 "wrong # of arguments in string-set");
    
    /* Side effetct no result is returned */
    string_set(car(arguments), 
               car(cdr(arguments)), 
               car(cdr(cdr(arguments))));

    return mk_none();
}

MAKE_WRAPPER_TWO_ARGS(string_eq);
MAKE_WRAPPER_TWO_ARGS(string_ci_eq);

static 
TYPE* 
_string_less_predicate_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_string_greater_predicate_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_string_less_equal_predicate_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_string_greater_equal_predicate_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_string_ci_less_predicate_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_string_ci_greater_predicate_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_string_ci_less_equal_predicate_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_string_ci_greater_equal_predicate_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_substring_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(length(arguments) == 3,
                 APPLY_ERROR,
                 "wrong # of arguments in substring");

    return substring(car(arguments), 
                     car(cdr(arguments)), 
                     car(cdr(cdr(arguments))));
}

static 
TYPE* 
_string_append_procedure_(const TYPE* arguments, const TYPE* env)
{
    return fold_right(&string_append, 
                      arguments, 
                      mk_string_with_length("", 0)); 
}


MAKE_WRAPPER_ONE_ARG(string_to_list)
MAKE_WRAPPER_ONE_ARG(list_to_string)

static 
TYPE* 
_string_to_number_procedure_(const TYPE* arguments, const TYPE* env)
{
    int nargs = length(arguments);
    assert_throw(
	nargs == 1 ||  nargs == 2,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in string->number");

    if (nargs == 1)
    {
	return string_to_number(car(arguments), mk_number_from_int(10));
    }
    else
    {
	return string_to_number(car(arguments), car(cdr(arguments)));
    }
}

static 
TYPE* 
_string_copy_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_string_fill_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

MAKE_PREDICATE_WRAPPER_ONE_ARG(is_vector);

static 
TYPE* 
_make_vector_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 1 ||  length(arguments) == 2,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in make-vector");
    
    if (length(arguments) == 1)
    {
        return mk_vector(car(arguments), nil());
    }
    else
    {
        return mk_vector(car(arguments), car(cdr(arguments)));
    }
}

static 
TYPE* 
_vector_procedure_(const TYPE* arguments, const TYPE* env)
{
    return list_to_vector(arguments);
}

MAKE_WRAPPER_ONE_ARG(vector_length);

static 
TYPE* 
_vector_ref_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 2,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in vector-ref");
    
    TYPE* k = car(cdr(arguments));

    assert_throw(is_number(k),
                 APPLY_ERROR,
                 "VECTOR_SET: second argument must be a number");
    

    return vector_ref(car(arguments), as_integer(k));
}

static 
TYPE* 
_vector_set_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 3,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in vector-set!");

    TYPE* k = car(cdr(arguments));
    
    assert_throw(is_number(k),
                 APPLY_ERROR,
                 "VECTOR_SET: second argument must be a number");

    vector_set(car(arguments), 
               as_integer(k), 
               car(cdr(cdr(arguments))));
    
    return mk_none();
}

static 
TYPE* 
_vector_to_list_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

MAKE_WRAPPER_ONE_ARG(list_to_vector);

static 
TYPE* 
_vector_fill_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert(FALSE);
    return NULL;
}

static 
TYPE* 
_quit_procedure_(const TYPE* arguments, const TYPE* env)
{
    exit(0);
    return NULL;
}

static 
TYPE* 
_read_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 0 || length(arguments) == 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in read");
    
    if (length(arguments) == 0)
    {
        return scm_read();
    }
    else
    {
        return read_from_port(car(arguments));
    }
}

static 
TYPE* 
_read_sml_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 0 || length(arguments) == 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in read-sml");
    
    if (length(arguments) == 0)
    {
        return read_sml();
    }
    else
    {
        return read_sml_from_port(car(arguments));
    }
}

static 
TYPE* 
_read_char_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 0 || length(arguments) == 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in read");
    
    if (length(arguments) == 0)
    {
        return read_char();
    }
    else
    {
        return read_char_from_port(car(arguments));
    }
}

static 
TYPE* 
_peek_char_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 0 || length(arguments) == 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in read");
    
    if (length(arguments) == 0)
    {
        return peek_char();
    }
    else
    {
        return peek_char_from_port(car(arguments));
    }
}

MAKE_VOID_WRAPPER_TWO_ARG(write_char_to_port);

MAKE_PREDICATE_WRAPPER_ONE_ARG(is_eof_object);
MAKE_VOID_WRAPPER_ONE_ARG(display);
MAKE_VOID_WRAPPER_NO_ARG(newline);

static 
TYPE* 
_error_procedure_(const TYPE* arguments, const TYPE* env)
{
    error(arguments);

    return  mk_none();
}

MAKE_WRAPPER_ONE_ARG(is_port);
MAKE_WRAPPER_ONE_ARG(is_input_port);
MAKE_WRAPPER_ONE_ARG(is_output_port);
MAKE_WRAPPER_ONE_ARG(open_input_file);
MAKE_WRAPPER_ONE_ARG(open_output_file);
MAKE_VOID_WRAPPER_ONE_ARG(close_input_port);
MAKE_VOID_WRAPPER_ONE_ARG(close_output_port);

/* unix */
MAKE_VOID_WRAPPER_TWO_ARG(nano_sleep);
MAKE_WRAPPER_TWO_ARGS(mk_udp_socket);
MAKE_WRAPPER_TWO_ARGS(udp_socket_recv);

static
TYPE*
_udp_socket_sendto_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 4,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in udp-socket-send");

    udp_socket_sendto(car(arguments),
                      car(cdr(arguments)),
                      car(cdr(cdr(arguments))),
                      car(cdr(cdr(cdr(arguments)))));

    return mk_none();
}

MAKE_WRAPPER_TWO_ARGS(mk_server_socket);
MAKE_WRAPPER_ONE_ARG(server_socket_accept);
MAKE_WRAPPER_TWO_ARGS(mk_tcp_socket);
MAKE_WRAPPER_TWO_ARGS(tcp_socket_recv);

static 
TYPE* 
_tcp_socket_send_procedure_(const TYPE* arguments, const TYPE* env) 
{
      assert_throw(
          length(arguments) == 2,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # "
        "of arguments in tcp-socket-send");

      tcp_socket_send(car(arguments), car(cdr(arguments)));
      
      return mk_none();
}
MAKE_VOID_WRAPPER_ONE_ARG(socket_close);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_blob);
MAKE_WRAPPER_ONE_ARG(mk_blob);
MAKE_WRAPPER_ONE_ARG(blob_length);
MAKE_WRAPPER_TWO_ARGS(blob_u8_ref);

static 
TYPE* 
_blob_u8_set_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 3,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in blob-u8-set!");
    
    blob_u8_set(car(arguments), 
                car(cdr(arguments)), 
                car(cdr(cdr(arguments))));

    return mk_none();
}

static
TYPE*
_gr_open_procedure_(const TYPE* arguments, const TYPE* env)
{
    unsigned int len = length(arguments);
    
    assert_throw(
        len == 0 || len== 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in gr-open");
    
    if (len == 1)
    {
        assert_throw(
            is_list(car(arguments)),
            APPLY_ERROR,
            "APPLY_PRIMITIVE_PROCEDURE: argument to gr-open must be a association list");
        
        gr_open(car(arguments));
    }
    else
    {
        gr_open(nil());
    }

    return mk_none();
}

static
TYPE*
_gr_close_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in gr-open");

    gr_close();

    return mk_none();
}

static 
TYPE* 
_gr_move_to_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 2,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in gr-move-to");

    gr_move_to(car(arguments), car(cdr(arguments)));

    return mk_none();
} 

MAKE_VOID_WRAPPER_ONE_ARG(gr_draw_char);
MAKE_VOID_WRAPPER_ONE_ARG(gr_draw_string);
MAKE_VOID_WRAPPER_ONE_ARG(gr_set_font);
MAKE_VOID_WRAPPER_ONE_ARG(gr_set_text_size);
MAKE_WRAPPER_ONE_ARG(gr_text_size);
MAKE_VOID_WRAPPER_NO_ARG(gr_draw_point);

static 
TYPE* 
_gr_draw_line_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 4,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in gr-move-to");

    gr_draw_line(car(arguments),
				 car(cdr(arguments)),
				 car(cdr(cdr(arguments))),
				 car(cdr(cdr(cdr(arguments)))));

    return mk_none();
} 

MAKE_VOID_WRAPPER_ONE_ARG(gr_set_foreground);

static 
TYPE* 
_x_events_queued_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in x-next-event");
    
    return x_events_queued();
}


static 
TYPE* 
_x_next_event_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in x-next-event");
    
    return x_next_event();
}

static 
TYPE* 
_x_flush_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in x-flush");

    x_flush();
    
    return mk_none();
}

MAKE_VOID_WRAPPER_TWO_ARG(gr_fill_rect);
MAKE_VOID_WRAPPER_ONE_ARG(gr_fill_polygon);

static 
TYPE* 
_gr_fill_arc_procedure_(const TYPE* arguments, const TYPE* env)
{
    assert_throw(
        length(arguments) == 4,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in gr-fill-arc");
 
    gr_fill_arc(car(arguments),
				car(cdr(arguments)),
				car(cdr(cdr(arguments))),
				car(cdr(cdr(cdr(arguments)))));

    return mk_none();
}

MAKE_VOID_WRAPPER_NO_ARG(gr_swap_buffers);

void 
init_primitive_procedures()
{
    _primitive_procedure_table_ = mk_hash_table(is_symbol_eq, _symbol_hash_);

    ADD_PROCEDURE(debug, debug);
    
    ADD_PROCEDURE(data_eval, eval);
    ADD_PROCEDURE(is_pair, pair?);
	ADD_PROCEDURE(is_procedure, procedure?);
    ADD_PROCEDURE(cons, cons);
    ADD_PROCEDURE(car, car);
    ADD_PROCEDURE(cdr, cdr);
    ADD_PROCEDURE(caar, caar);
    ADD_PROCEDURE(cadr, cadr);
    ADD_PROCEDURE(caadr, caadr);
    ADD_PROCEDURE(caddr, caddr);
    ADD_PROCEDURE(cddr, cddr);
    ADD_PROCEDURE(cdar, cdar);
    ADD_PROCEDURE(cdadr, cdadr);
    ADD_PROCEDURE(cdddr,cdddr);
    ADD_PROCEDURE(cadddr, cadddr);
    ADD_PROCEDURE(set_car, set-car!);
    ADD_PROCEDURE(set_cdr, set-cdr!);
    ADD_PROCEDURE(IS_NIL, null?);
	ADD_PROCEDURE(is_list, list?);
    ADD_PROCEDURE(list, list);
	ADD_PROCEDURE(length, length);
    ADD_PROCEDURE(reverse, reverse);
    ADD_PROCEDURE(unzip, unzip);

    /* symbols */
    ADD_PROCEDURE(is_symbol, symbol?);
    ADD_PROCEDURE(symbol_to_string, symbol->string);
    ADD_PROCEDURE(string_to_symbol, string->symbol);

    /* characters */
    ADD_PROCEDURE(char_to_integer, char->integer);
    ADD_PROCEDURE(integer_to_char, integer->char);
    ADD_PROCEDURE(is_char, char?);

    /* equivalences */
    ADD_PROCEDURE(is_eq, eq?);
    ADD_PROCEDURE(is_eqv, eqv?);	
    ADD_PROCEDURE(is_equal, equal?);
    
    /* number */
    ADD_PROCEDURE(number_to_string, number->string);
    ADD_PROCEDURE(is_number, number?);
    ADD_PROCEDURE(is_complex, complex?);
    ADD_PROCEDURE(is_real, real?);
    ADD_PROCEDURE(is_rational, rational?);
    ADD_PROCEDURE(is_integer, integer?);
    ADD_PROCEDURE(is_exact, exact?);
    ADD_PROCEDURE(is_inexact, inexact?);
    ADD_PROCEDURE(equal, =);
    ADD_PROCEDURE(less, <);
    ADD_PROCEDURE(greater, >);
    ADD_PROCEDURE(less_equal, <=);
    ADD_PROCEDURE(greater_equal, >=);
    ADD_PROCEDURE(is_number_zero, zero?);
    ADD_PROCEDURE(is_number_positive, positive?);
    ADD_PROCEDURE(is_number_negative, negative?);
    ADD_PROCEDURE(is_number_odd, odd?);
    ADD_PROCEDURE(is_number_even, even?);
    ADD_PROCEDURE(max, max?);
    ADD_PROCEDURE(min, min?);
    ADD_PROCEDURE(plus, +);
    ADD_PROCEDURE(mul, *);
    ADD_PROCEDURE(minus, -);
    ADD_PROCEDURE(div, /);
    ADD_PROCEDURE(abs, abs);
    ADD_PROCEDURE(quotient, quotient);
    ADD_PROCEDURE(modulo, modulo);
	ADD_PROCEDURE(remainder_number, remainder);
    ADD_PROCEDURE(gcd, gcd);
    ADD_PROCEDURE(lcm, lcm);
	ADD_PROCEDURE(round_number, round);
	ADD_PROCEDURE(sin, sin);
	ADD_PROCEDURE(cos, cos);
	ADD_PROCEDURE(sqrt, sqrt);

    /* boolean */
    ADD_PROCEDURE(is_boolean, boolean?);
    ADD_PROCEDURE(not, not);

    /* string */
    ADD_PROCEDURE(is_string, string?);
    ADD_PROCEDURE(make_string, make-string);
    ADD_PROCEDURE(string, string);
    ADD_PROCEDURE(string_length, string-length);
    ADD_PROCEDURE(string_ref, string-ref);
    ADD_PROCEDURE(string_set, string-set!);
    ADD_PROCEDURE(string_eq, string=?);
    ADD_PROCEDURE(string_ci_eq, string-ci=?);
    ADD_PROCEDURE(string_less_predicate, string<?);
    ADD_PROCEDURE(string_greater_predicate, string>?);
    ADD_PROCEDURE(string_less_equal_predicate, string<=?);
    ADD_PROCEDURE(string_greater_equal_predicate, string>=?);
    ADD_PROCEDURE(string_ci_less_predicate, string-ci<?);
    ADD_PROCEDURE(string_ci_greater_predicate, string-ci>?);
    ADD_PROCEDURE(string_ci_less_equal_predicate, string-ci<=?);
    ADD_PROCEDURE(string_ci_greater_equal_predicate, string-ci>=?);
    ADD_PROCEDURE(substring, substring);
    ADD_PROCEDURE(string_append, string-append);
    ADD_PROCEDURE(string_to_list, string->list);
    ADD_PROCEDURE(list_to_string, list->string);
    ADD_PROCEDURE(string_to_number, string->number);
    ADD_PROCEDURE(string_copy,string-copy);
    ADD_PROCEDURE(string_fill, string-fill!);

    /* vectors */
    ADD_PROCEDURE(is_vector, vector?);
    ADD_PROCEDURE(make_vector, make-vector);
    ADD_PROCEDURE(vector, vector);
    ADD_PROCEDURE(vector_length, vector-length);
    ADD_PROCEDURE(vector_ref, vector-ref);
    ADD_PROCEDURE(vector_set, vector-set!);
    ADD_PROCEDURE(vector_to_list, vector->list);
    ADD_PROCEDURE(list_to_vector, list->vector);
    ADD_PROCEDURE(vector_fill, vector-fill!);

    /* io */
    ADD_PROCEDURE(quit, quit);
    ADD_PROCEDURE(read, read);
	ADD_PROCEDURE(read_sml, read-sml);
    ADD_PROCEDURE(read_char, read-char);
    ADD_PROCEDURE(peek_char, peek-char);
    ADD_PROCEDURE(write_char_to_port, write-char);
    ADD_PROCEDURE(is_eof_object, eof-object?);
    ADD_PROCEDURE(display, display);
    ADD_PROCEDURE(newline, newline);
    ADD_PROCEDURE(error, error);
    ADD_PROCEDURE(is_port, port?);
    ADD_PROCEDURE(is_input_port, input-port?);
    ADD_PROCEDURE(is_output_port, output-port?);
    ADD_PROCEDURE(open_input_file, open-input-file);
    ADD_PROCEDURE(open_output_file, open-output-file);
    ADD_PROCEDURE(close_input_port, close-input-port);
    ADD_PROCEDURE(close_output_port, close-output-port);

    /* unix */
    ADD_PROCEDURE(nano_sleep, nano-sleep);

    /* sockets */
    ADD_PROCEDURE(mk_udp_socket, make-udp-socket);
    ADD_PROCEDURE(udp_socket_recv, udp-socket-recv!);
    ADD_PROCEDURE(udp_socket_sendto, udp-socket-sendto);
    ADD_PROCEDURE(mk_server_socket, make-server-socket);
    ADD_PROCEDURE(server_socket_accept, server-socket-accept); 
    ADD_PROCEDURE(mk_tcp_socket, make-tcp-socket);
    ADD_PROCEDURE(tcp_socket_recv, tcp-socket-recv!); 
    ADD_PROCEDURE(tcp_socket_send, tcp-socket-send);
    ADD_PROCEDURE(socket_close, socket-close);

    /* blobs */
    ADD_PROCEDURE(is_blob, blob?);
    ADD_PROCEDURE(mk_blob, make-blob);
    ADD_PROCEDURE(blob_length, blob-length);
    ADD_PROCEDURE(blob_u8_ref, blob-u8-ref);
    ADD_PROCEDURE(blob_u8_set, blob-u8-set!);

    /* graphics */
    ADD_PROCEDURE(gr_open, gr-open);
    ADD_PROCEDURE(gr_close, gr-close);
    ADD_PROCEDURE(gr_move_to, gr-move-to!);
    ADD_PROCEDURE(gr_draw_char, gr-draw-char);
    ADD_PROCEDURE(gr_draw_string, gr-draw-string);
    ADD_PROCEDURE(gr_set_font, gr-set-font!);
    ADD_PROCEDURE(gr_set_text_size, gr-set-text!);
    ADD_PROCEDURE(gr_text_size, gr-text-size);
	ADD_PROCEDURE(gr_draw_point, gr-draw-point);
	ADD_PROCEDURE(gr_draw_line, gr-draw-line);
    ADD_PROCEDURE(gr_set_foreground, gr-set-foreground);
    ADD_PROCEDURE(gr_swap_buffers, gr-swap-buffers);
    ADD_PROCEDURE(x_events_queued, x-events-queued);
    ADD_PROCEDURE(x_next_event, x-next-event);
    ADD_PROCEDURE(x_flush, x-flush);
    ADD_PROCEDURE(gr_fill_arc, gr-fill-arc);
	ADD_PROCEDURE(gr_fill_rect, gr-fill-rect);
    ADD_PROCEDURE(gr_fill_polygon, gr-fill-polygon);
}

int
is_primitive_procedure(const TYPE* procedure)
{
    return procedure->type == PRIMITIVE_PROCEDURE;
}

TYPE* 
find_primitive_procedure(const TYPE* symbol)
{
    if (!is_symbol(symbol)) {
		fprintf(stderr, "FIND_PRIMITIVE_PROCEDURE: not a symbol");
		display_debug(symbol);
		return nil();
	}

    return hash_table_ref(_primitive_procedure_table_, symbol);
}

TYPE* 
apply_primitive_procedure(const TYPE* procedure, 
                          const TYPE* arguments,
                          const TYPE* env)
{
    TYPE* result;

    assert(!IS_NIL(procedure) && 
           procedure->type == PRIMITIVE_PROCEDURE &&
           "APPLY_PRIMITIVE_PROCEDURE: wrong type of stored procedure");
    
    result = procedure->d.f->f(arguments, env);

    return result;
}
