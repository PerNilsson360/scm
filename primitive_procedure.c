#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

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

struct FUNCTION
{
    type* (*f) (const type* arguments, const type* env);
};
typedef struct FUNCTION  FUNCTION;

int _debug_ = 0;

static type* _debug_symbol_;
static type* _pair_predicate_symbol_;
static type* _cons_symbol_;
static type* _car_symbol_;
static type* _cdr_symbol_;
static type* _set_car_symbol_;
static type* _set_cdr_symbol_;
static type* _null_predicate_symbol_;
static type* _symbol_predicate_symbol_;
static type* _symbol_to_string_symbol_;
static type* _string_to_symbol_symbol_;
static type* _number_to_string_symbol_;
static type* _eq_predicate_symbol_;
static type* _eqv_predicate_symbol_;
static type* _number_predicate_symbol_;
static type* _complex_predicate_symbol_;
static type* _real_predicate_symbol_;
static type* _rational_predicate_symbol_;
static type* _integer_predicate_symbol_;
static type* _exact_predicate_symbol_;
static type* _inexact_predicate_symbol_;
static type* _equal_symbol_;
static type* _less_symbol_;
static type* _greater_symbol_;
static type* _less_equal_symbol_;
static type* _greater_equal_symbol_;
static type* _zero_predicate_symbol_;
static type* _positive_predicate_symbol_;
static type* _negative_predicate_symbol_;
static type* _odd_predicate_symbol_;
static type* _even_predicate_symbol_;
static type* _max_symbol_;
static type* _min_symbol_;
static type* _plus_symbol_;
static type* _mul_symbol_;
static type* _minus_symbol_;
static type* _div_symbol_;
static type* _abs_symbol_;
static type* _quotient_symbol_;
static type* _remainder_symbol_;
static type* _modulo_symbol_;
static type* _gcd_symbol_;
static type* _lcm_symbol_;
static type* _boolean_predicate_symbol_;
static type* _not_symbol_;

static type* _make_string_symbol_;

static type* _primitive_procedure_table_;

#define MAKE_VOID_WRAPPER_NO_ARG(c_name)                                \
static                                                                  \
type*                                                                   \
_ ## c_name ## _ ## procedure ## _(const type* arguments, const type* env)    \
{                                                                       \
    assert_throw(length(arguments) == 0,                                \
                 APPLY_ERROR,                                           \
                 "wrong # of arguments in " #c_name);                   \
    c_name();                                                           \
    return mk_none();                                                   \
}                                                                       \


#define MAKE_WRAPPER_ONE_ARG(c_name)                                    \
static                                                                  \
type*                                                                   \
_ ## c_name ## _ ## procedure ## _(const type* arguments, const type* env)    \
{                                                                       \
    assert_throw(length(arguments) == 1,                                \
                 APPLY_ERROR,                                           \
                 "wrong # of arguments in " #c_name);                   \
    return c_name(car(arguments));                                      \
}                                                                       \

#define MAKE_VOID_WRAPPER_ONE_ARG(c_name)                                    \
static                                                                  \
type*                                                                   \
_ ## c_name ## _ ## procedure ## _(const type* arguments, const type* env)    \
{                                                                       \
    assert_throw(length(arguments) == 1,                                \
                 APPLY_ERROR,                                           \
                 "wrong # of arguments in " #c_name);                   \
    c_name(car(arguments));                                             \
    return mk_none();                                                   \
}                                                                       \

#define MAKE_PREDICATE_WRAPPER_ONE_ARG(c_name)          \
static                                                  \
type*                                                   \
_ ## c_name ## _ ## procedure ## _(const type* arguments, const type* env)  \
{                                                       \
    assert_throw(length(arguments) == 1,                \
                 APPLY_ERROR,                           \
                 "wrong # of arguments in " #c_name);   \
    return mk_boolean(c_name(car(arguments)));          \
}                                                       \


#define MAKE_WRAPPER_TWO_ARGS(c_name)                   \
static                                                  \
type*                                                   \
_ ## c_name ## _ ## procedure ## _(const type* arguments, const type* env)  \
{                                                       \
    assert_throw(length(arguments) == 2,                \
                 APPLY_ERROR,                           \
                 "wrong # of arguments in " #c_name);   \
    return c_name(car(arguments), car(cdr(arguments))); \
}                                                       \

#define MAKE_PREDICATE_WRAPPER_TWO_ARGS(c_name)         \
static                                                  \
type*                                                   \
_ ## c_name ## _ ## procedure ## _(const type* arguments, const type* env)  \
{                                                       \
    assert_throw(length(arguments) == 2,                \
                 APPLY_ERROR,                           \
                 "wrong # of arguments in " #c_name);   \
    return mk_boolean(c_name(car(arguments), car(cdr(arguments)))); \
}                                                       \


#define ADD_PROCEDURE(c_name, scheme_name)                              \
    hash_table_set(_primitive_procedure_table_,                         \
                   mk_symbol(#scheme_name),                             \
                   mk_primitive_procedure(_ ## c_name ## _ ## procedure ## _))

static
type* 
mk_primitive_procedure(type* (f) (const type* arguments, const type* env))
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, 
                "MK_PRIMITIVE_PROCEDURE: could not allocate "
                "memory for type");
        exit(1);
    }

    result->data = mloc(sizeof(FUNCTION));

    if (result->data == NULL)
    {
        fprintf(stderr, "MK_PRIMITIVE_PROCEDURE: could not allocate "
                "memory for FUNCTION");
        exit(1);
    }
    
    result->type = PRIMITIVE_PROCEDURE;
    ((FUNCTION*)result->data)->f = f;

    return result;
}

static 
type* 
_debug_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in debug");

    _debug_ = is_true(car(arguments));

    return mk_none();
}

MAKE_WRAPPER_TWO_ARGS(eval);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_pair);
MAKE_WRAPPER_TWO_ARGS(cons);
MAKE_WRAPPER_ONE_ARG(car);
MAKE_WRAPPER_ONE_ARG(cdr);

static 
type* 
_set_car_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) == 2,
                 APPLY_ERROR,
                 "wrong # of arguments in set-car!");
        
    set_car(car(arguments), car(cdr(arguments)));
    return mk_none();
}

static 
type* 
_set_cdr_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) == 2,
                 APPLY_ERROR,
                 "wrong # of arguments in set-cdr!");
    
    set_cdr(car(arguments), car(cdr(arguments)));
    return mk_none();
}

MAKE_PREDICATE_WRAPPER_ONE_ARG(is_nil);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_symbol);
MAKE_WRAPPER_ONE_ARG(symbol_to_string);
MAKE_WRAPPER_ONE_ARG(string_to_symbol);
MAKE_WRAPPER_ONE_ARG(char_to_integer);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_char);
MAKE_WRAPPER_ONE_ARG(number_to_string);
MAKE_PREDICATE_WRAPPER_TWO_ARGS(is_eq);
MAKE_PREDICATE_WRAPPER_TWO_ARGS(is_eqv);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_number);


static 
type* 
_complex_predicate_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in complex?");
    
    return mk_boolean(is_number(car(arguments)));
}

static 
type* 
_real_predicate_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in real?");
    
   return mk_boolean(FALSE); /* not implemented */
}

static 
type* 
_rational_predicate_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in rational?");

    return mk_boolean(FALSE); /* not implemented */
}

static 
type* 
_integer_predicate_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) == 1,
                 APPLY_ERROR,
                 "wrong # of arguments in integer?");
    
    return mk_boolean(is_number(car(arguments))); /* only int impl */
}

static 
type* 
_exact_predicate_procedure_(const type* arguments, const type* env)
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
type* 
_inexact_predicate_procedure_(const type* arguments, const type* env)
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
type* 
_equal_procedure_(const type* arguments, const type* env)
{
    return it_bin_pred(arguments, &is_number_equal);
}

static 
type* 
_less_procedure_(const type* arguments, const type* env)
{
    return it_bin_pred(arguments, &is_number_lt);
}

static 
type* 
_greater_procedure_(const type* arguments, const type* env)
{
    return it_bin_pred(arguments, &is_number_gt);
}

static 
type* 
_less_equal_procedure_(const type* arguments, const type* env)
{
    return it_bin_pred(arguments, &is_number_lt_eq);
}

static 
type* 
_greater_equal_procedure_(const type* arguments, const type* env)
{
    return it_bin_pred(arguments, &is_number_gt_eq);
}

MAKE_WRAPPER_ONE_ARG(is_number_zero);
MAKE_WRAPPER_ONE_ARG(is_number_positive);
MAKE_WRAPPER_ONE_ARG(is_number_negative);
MAKE_WRAPPER_ONE_ARG(is_number_odd);
MAKE_WRAPPER_ONE_ARG(is_number_even);

static 
type* 
_max_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) > 0,
                 APPLY_ERROR,
                 "wrong # of arguments in max");

    return fold_right(&max_number, arguments, car(arguments));
}

static 
type* 
_min_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) > 0,
                 APPLY_ERROR,
                 "wrong # of arguments in min");

    return fold_right(&min_number, arguments, car(arguments));
}

static 
type* 
_plus_procedure_(const type* arguments, const type* env)
{
    return fold_right(&add_number, arguments, mk_number("0", 1, TRUE)); 
}

static 
type* 
_mul_procedure_(const type* arguments, const type* env)
{
    return fold_right(&mul_number, arguments, mk_number("1", 1, TRUE)); 
}

static 
type* 
_minus_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) > 0,
                 APPLY_ERROR,
                 "wrong # of arguments in -");

    return sub_numbers(arguments); 
}

static 
type* 
_div_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) > 0,
                 APPLY_ERROR,
                 "wrong # of arguments in /");

    return div_numbers(arguments); 
}

static 
type* 
_abs_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_quotient_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

MAKE_WRAPPER_TWO_ARGS(remainder_number);

static 
type* 
_modulo_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_gcd_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_lcm_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

MAKE_PREDICATE_WRAPPER_ONE_ARG(is_boolean);
MAKE_WRAPPER_ONE_ARG(not);
MAKE_PREDICATE_WRAPPER_ONE_ARG(is_string);

static 
type* 
_make_string_procedure_(const type* arguments, const type* env)
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
type* 
_string_procedure_(const type* arguments, const type* env)
{
    return mk_string_from_chars(arguments);
}

MAKE_WRAPPER_ONE_ARG(string_length);
MAKE_WRAPPER_TWO_ARGS(string_ref);

static 
type* 
_string_set_procedure_(const type* arguments, const type* env)
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
type* 
_string_less_predicate_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_greater_predicate_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_less_equal_predicate_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_greater_equal_predicate_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_ci_less_predicate_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_ci_greater_predicate_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_ci_less_equal_predicate_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_ci_greater_equal_predicate_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_substring_procedure_(const type* arguments, const type* env)
{
    assert_throw(length(arguments) == 3,
                 APPLY_ERROR,
                 "wrong # of arguments in substring");

    return substring(car(arguments), 
                     car(cdr(arguments)), 
                     car(cdr(cdr(arguments))));
}

static 
type* 
_string_append_procedure_(const type* arguments, const type* env)
{
    return fold_right(&string_append, 
                      arguments, 
                      mk_string_with_length("", 0)); 
}

static 
type* 
_string_to_list_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_copy_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_string_fill_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

MAKE_PREDICATE_WRAPPER_ONE_ARG(is_vector);

static 
type* 
_make_vector_procedure_(const type* arguments, const type* env)
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
type* 
_vector_procedure_(const type* arguments, const type* env)
{
    return list_to_vector(arguments);
}

MAKE_WRAPPER_ONE_ARG(vector_length);

static 
type* 
_vector_ref_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 2,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in vector-ref");
    
    type* k = car(cdr(arguments));

    assert_throw(is_number(k),
                 APPLY_ERROR,
                 "VECTOR_SET: second argument must be a number");
    

    return vector_ref(car(arguments), (int) k->data);
}

static 
type* 
_vector_set_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 3,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in vector-set!");

    type* k = car(cdr(arguments));
    
    assert_throw(is_number(k),
                 APPLY_ERROR,
                 "VECTOR_SET: second argument must be a number");

    vector_set(car(arguments), 
               (int) k->data, 
               car(cdr(cdr(arguments))));
    
    return mk_none();
}

static 
type* 
_vector_to_list_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

MAKE_WRAPPER_ONE_ARG(list_to_vector);

static 
type* 
_vector_fill_procedure_(const type* arguments, const type* env)
{
    assert(FALSE);
    return NULL;
}

static 
type* 
_quit_procedure_(const type* arguments, const type* env)
{
    exit(0);
    return NULL;
}

static 
type* 
_read_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 0 || length(arguments) == 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in read");
    
    if (length(arguments) == 0)
    {
        return read();
    }
    else
    {
        return read_from_port(car(arguments));
    }
}

static 
type* 
_read_char_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 0 || length(arguments) == 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in read");
    
    if (length(arguments) == 0)
    {
        assert(FALSE);
    }
    else
    {
        return read_char_from_port(car(arguments));
    }
}

static 
type* 
_peek_char_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 0 || length(arguments) == 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in read");
    
    if (length(arguments) == 0)
    {
        assert(FALSE);
    }
    else
    {
        return peek_char_from_port(car(arguments));
    }
}

MAKE_PREDICATE_WRAPPER_ONE_ARG(is_eof_object);
MAKE_VOID_WRAPPER_ONE_ARG(display);
MAKE_VOID_WRAPPER_NO_ARG(newline);

static 
type* 
_error_procedure_(const type* arguments, const type* env)
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

static 
type* 
_nano_sleep_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 2,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in nano-sleep");

    nano_sleep(car(arguments), car(cdr(arguments)));
    
    return  mk_none();
}

MAKE_WRAPPER_TWO_ARGS(mk_udp_socket);
MAKE_WRAPPER_TWO_ARGS(udp_socket_recv);

static
type*
_udp_socket_sendto_procedure_(const type* arguments, const type* env)
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
type* 
_tcp_socket_send_procedure_(const type* arguments, const type* env) 
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
type* 
_blob_u8_set_procedure_(const type* arguments, const type* env)
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
type*
_gr_open_procedure_(const type* arguments, const type* env)
{
    unsigned int len = length(arguments);
    
    assert_throw(
        len == 0 || len== 1,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in gr-open");
    
    if (len == 1)
    {
        assert_throw(
            is_true(is_list(car(arguments))),
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
type*
_gr_close_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in gr-open");

    gr_close();

    return mk_none();
}

static 
type* 
_gr_move_to_procedure_(const type* arguments, const type* env)
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
MAKE_VOID_WRAPPER_ONE_ARG(gr_set_foreground);

static 
type* 
_x_events_queued_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in x-next-event");
    
    return x_events_queued();
}


static 
type* 
_x_next_event_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in x-next-event");
    
    return x_next_event();
}

static 
type* 
_x_flush_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in x-flush");

    x_flush();
    
    return mk_none();
}

static 
type* 
_x_fill_arc_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 5,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in x-fill-arc");
 
    x_fill_arc(car(arguments),
               car(cdr(arguments)),
               car(cdr(cdr(arguments))),
               car(cdr(cdr(cdr(arguments)))),
               car(cdr(cdr(cdr(cdr(arguments))))));

    return mk_none();
}

static
type*
_gr_root_win_procedure_(const type* arguments, const type* env)
{
    assert_throw(
        length(arguments) == 0,
        APPLY_ERROR,
        "APPLY_PRIMITIVE_PROCEDURE: wrong # of arguments in gr-open");

    return gr_root_win();
}

void 
init_primitive_procedures()
{
    _primitive_procedure_table_ = mk_hash_table(is_symbol_eq, _symbol_hash_);

    ADD_PROCEDURE(debug, debug);
    
    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("eval"),
                   mk_primitive_procedure(_eval_procedure_));

    ADD_PROCEDURE(is_pair, pair?);

    _cons_symbol_ = mk_symbol("cons");
    hash_table_set(_primitive_procedure_table_, 
                   _cons_symbol_, 
                   mk_primitive_procedure(_cons_procedure_));


    _car_symbol_ = mk_symbol("car");
    hash_table_set(_primitive_procedure_table_, 
                   _car_symbol_, 
                   mk_primitive_procedure(_car_procedure_));

    _cdr_symbol_ = mk_symbol("cdr");
    hash_table_set(_primitive_procedure_table_, 
                   _cdr_symbol_, 
                   mk_primitive_procedure(_cdr_procedure_));

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("set-car!"), 
                   mk_primitive_procedure(_set_car_procedure_));

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("set-cdr!"), 
                   mk_primitive_procedure(_set_cdr_procedure_));

    ADD_PROCEDURE(is_nil, null?);
    ADD_PROCEDURE(is_symbol, symbol?);

    _symbol_to_string_symbol_ = mk_symbol("symbol->string");
    hash_table_set(_primitive_procedure_table_, 
                   _symbol_to_string_symbol_, 
                   mk_primitive_procedure(_symbol_to_string_procedure_));

    _string_to_symbol_symbol_ = mk_symbol("string->symbol");
    hash_table_set(_primitive_procedure_table_, 
                   _string_to_symbol_symbol_, 
                   mk_primitive_procedure(_string_to_symbol_procedure_));

    ADD_PROCEDURE(char_to_integer, char->integer);
    ADD_PROCEDURE(is_char, char?);

    _number_to_string_symbol_ = mk_symbol("number->string");
    hash_table_set(_primitive_procedure_table_, 
                   _number_to_string_symbol_, 
                   mk_primitive_procedure(_number_to_string_procedure_));

    ADD_PROCEDURE(is_eq, eq?);
    ADD_PROCEDURE(is_eqv, eqv?);
    ADD_PROCEDURE(is_number, number?);

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("complex?"), 
                   mk_primitive_procedure(_complex_predicate_procedure_));

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("real?"), 
                   mk_primitive_procedure(_real_predicate_procedure_));

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("rational?"), 
                   mk_primitive_procedure(_rational_predicate_procedure_));

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("integer?"), 
                   mk_primitive_procedure(_integer_predicate_procedure_));

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("exact?"), 
                   mk_primitive_procedure(_exact_predicate_procedure_));

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("inexact?"), 
                   mk_primitive_procedure(_inexact_predicate_procedure_));

    hash_table_set(_primitive_procedure_table_, 
                   mk_symbol("="), 
                   mk_primitive_procedure(_equal_procedure_));

    _less_symbol_ = mk_symbol("<");
    hash_table_set(_primitive_procedure_table_, 
                   _less_symbol_, 
                   mk_primitive_procedure(_less_procedure_));

    _greater_symbol_ = mk_symbol(">");
    hash_table_set(_primitive_procedure_table_, 
                   _greater_symbol_, 
                   mk_primitive_procedure(_greater_procedure_));

    _less_equal_symbol_ = mk_symbol("<=");
    hash_table_set(_primitive_procedure_table_, 
                   _less_equal_symbol_, 
                   mk_primitive_procedure(_less_equal_procedure_));

    _greater_equal_symbol_ = mk_symbol(">=");
    hash_table_set(_primitive_procedure_table_, 
                   _greater_equal_symbol_, 
                   mk_primitive_procedure(_greater_equal_procedure_));

    ADD_PROCEDURE(is_number_zero, zero?);
    ADD_PROCEDURE(is_number_positive, positive?);
    ADD_PROCEDURE(is_number_negative, negative?);
    ADD_PROCEDURE(is_number_odd, odd?);
    ADD_PROCEDURE(is_number_even, even?);

    _max_symbol_ = mk_symbol("max?");
    hash_table_set(_primitive_procedure_table_, 
                   _max_symbol_, 
                   mk_primitive_procedure(_max_procedure_));

    _min_symbol_ = mk_symbol("min?");
    hash_table_set(_primitive_procedure_table_, 
                   _min_symbol_, 
                   mk_primitive_procedure(_min_procedure_));

    _plus_symbol_ = mk_symbol("+");
    hash_table_set(_primitive_procedure_table_, 
                   _plus_symbol_, 
                   mk_primitive_procedure(_plus_procedure_));

    _mul_symbol_ = mk_symbol("*");
    hash_table_set(_primitive_procedure_table_, 
                   _mul_symbol_, 
                   mk_primitive_procedure(_mul_procedure_));

    _minus_symbol_ = mk_symbol("-");
    hash_table_set(_primitive_procedure_table_, 
                   _minus_symbol_, 
                   mk_primitive_procedure(_minus_procedure_));

    _div_symbol_ = mk_symbol("/");
    hash_table_set(_primitive_procedure_table_, 
                   _div_symbol_, 
                   mk_primitive_procedure(_div_procedure_));

    _abs_symbol_ = mk_symbol("abs");
    hash_table_set(_primitive_procedure_table_, 
                   _abs_symbol_, 
                   mk_primitive_procedure(_abs_procedure_));

    _quotient_symbol_ = mk_symbol("quotient");
    hash_table_set(_primitive_procedure_table_, 
                   _quotient_symbol_, 
                   mk_primitive_procedure(_quotient_procedure_));

    ADD_PROCEDURE(remainder_number, remainder);

    _modulo_symbol_ = mk_symbol("modulo");
    hash_table_set(_primitive_procedure_table_, 
                   _modulo_symbol_, 
                   mk_primitive_procedure(_modulo_procedure_));

    _gcd_symbol_ = mk_symbol("gcd");
    hash_table_set(_primitive_procedure_table_, 
                   _gcd_symbol_, 
                   mk_primitive_procedure(_gcd_procedure_));

    _lcm_symbol_ = mk_symbol("lcm");
    hash_table_set(_primitive_procedure_table_, 
                   _lcm_symbol_, 
                   mk_primitive_procedure(_lcm_procedure_));

    ADD_PROCEDURE(is_boolean, boolean?);
    
    _not_symbol_ = mk_symbol("not");
    hash_table_set(_primitive_procedure_table_, 
                   _not_symbol_, 
                   mk_primitive_procedure(_not_procedure_));

    ADD_PROCEDURE(is_string, string?);

    _make_string_symbol_ = mk_symbol("make-string");
    hash_table_set(_primitive_procedure_table_, 
                   _make_string_symbol_, 
                   mk_primitive_procedure(_make_string_procedure_));
    
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
    ADD_PROCEDURE(read_char, read-char);
    ADD_PROCEDURE(peek_char, peek-char);
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
    ADD_PROCEDURE(gr_set_foreground, gr-set-foreground);
    ADD_PROCEDURE(gr_root_win, gr-root-win);
    ADD_PROCEDURE(x_events_queued, x-events-queued);
    ADD_PROCEDURE(x_next_event, x-next-event);
    ADD_PROCEDURE(x_flush, x-flush);
    ADD_PROCEDURE(x_fill_arc, x-fill-arc);
}

int
is_symbol_primitive_procedure(const type* procedure)
{
    return 
        is_symbol(procedure) 
        && !is_nil(hash_table_ref(_primitive_procedure_table_, procedure));
}

int
is_primitive_procedure(const type* procedure)
{
    return !is_nil(procedure) && procedure->type == PRIMITIVE_PROCEDURE;
}

type* 
find_primitive_procedure(const type* symbol)
{
    type* result;
    
    assert(is_symbol(symbol) && "symbol must be a symbol");

    result = hash_table_ref(_primitive_procedure_table_, symbol);

    assert(!is_nil(result) && "could not find primitive procedure");
    
    return result;
}

type* 
apply_primitive_procedure(const type* procedure, 
                          const type* arguments,
                          const type* env)
{
    type* result;

    assert(!is_nil(procedure) && 
           procedure->type == PRIMITIVE_PROCEDURE &&
           "APPLY_PRIMITIVE_PROCEDURE: wrong type of stored procedure");
    
    result = ((FUNCTION*)procedure->data)->f(arguments, env);

    return result;
}
