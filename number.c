#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>

#include <gc.h>

#include "str.h"
#include "symbol.h"
#include "error.h"
#include "common.h"
#include "number.h"
#include "util.h"

static type* 
mk_unasigned_number()
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_NUMBER: could not allocate memory for type");
        exit(1);
    }
    
    result->type = NUMBER;

    return result;
}

type* mk_number_from_int(int n)
{
    type* result = mk_unasigned_number();

    result->data = (void*) n;

    return result;
}

type* 
mk_number(const char* symbol, unsigned int length, int positive)
{
    type* result = mk_unasigned_number();
    int number = 0;
    unsigned int i = 0;
        
    for(i = 0; i < length; i++)
    {
        number += symbol[i] - '0';

        if (i < length - 1)
        {
            number *= 10;
        }
    }

    result->data = (positive ? (void*) number : (void*) (- number));

    return result;
}

type* 
mk_hex_number(const char* symbol, unsigned int length)
{
    type* result = mk_unasigned_number();
    int number = 0;
    unsigned int i = 0;
        
    for(i = 0; i < length; i++)
    {
        switch (symbol[i]) 
        {
        case 'a':
        case 'A':
            number += 10;
            break;
        case 'b':
        case 'B':
            number += 11;
            break;
        case 'c':
        case 'C':
            number += 12;
            break;
        case 'd':
        case 'D':
            number += 13;
            break;
        case 'e':
        case 'E':
            number += 14;
            break;
        case 'f':
        case 'F':
            number += 15;
            break;
        default:
            number += symbol[i] - '0';
        }

        if (i < length - 1)
        {
            number *= 16;
        }
    }

    result->data = (void*) number;

    return result;
}

int
is_number(const type* number)
{
    return number->type == NUMBER;
}

int
is_number_equal(const type* left, const type* right)
{
    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "IS_NUMBER_EQUAL: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "IS_NUMBER_EQUAL: right must be a number");

    return (int) left->data == (int) right->data;
}

int
is_number_lt(const type* left, const type* right)
{
    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "IS_NUMBER_LT: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "IS_NUMBER_LT: right must be a number");

    return (int) left->data < (int) right->data;
}

int
is_number_gt(const type* left, const type* right)
{
    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "IS_NUMBER_GT: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "IS_NUMBER_GT: right must be a number");

    return (int) left->data > (int) right->data;
}

int
is_number_lt_eq(const type* left, const type* right)
{
    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "IS_NUMBER_LT_EQUAL: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "IS_NUMBER_LT_EQUAL: right must be a number");

    return (int) left->data <= (int) right->data;
}

int
is_number_gt_eq(const type* left, const type* right)
{
    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "IS_NUMBER_GT_EQUAL: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "IS_NUMBER_GT_EQUAL: right must be a number");

    return (int) left->data >= (int) right->data;
}

type* 
is_number_zero(const type* n)
{
    assert_throw(is_number(n),
		 TYPE_ERROR,
		 "IS_NUMBER_ZERO: n must be a number");
    return mk_boolean(((int) n->data) == 0);
}

type* 
is_number_positive(const type* n)
{
    assert_throw(is_number(n),
		 TYPE_ERROR,
		 "IS_NUMBER_POSITIVE: n must be a number");

    return mk_boolean(((int) n->data) > 0);
}

type* is_number_negative(const type* n)
{
    assert_throw(is_number(n),
		 TYPE_ERROR,
		 "IS_NUMBER_NEGATIVE: n must be a number");

    return mk_boolean(((int) n->data) > 0);
}

type* 
is_number_odd(const type* n)
{
    assert_throw(is_number(n),
		 TYPE_ERROR,
		 "IS_NUMBER_ODD: n must be a number");

    return mk_boolean((((int) n->data) % 2) == 1);
}

type* 
is_number_even(const type* n)
{
   assert_throw(is_number(n),
		TYPE_ERROR,
		"IS_NUMBER_EVEN: n must be a number");

    return mk_boolean((((int) n->data) % 2) == 0);
}

type* 
max_number(const type* left, const type* right)
{
    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "MAX_NUMBER: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "MAX_NUMBER: right must be a number");

    return  (type*) (((int) left->data) > ((int) right->data) ? left : right);
}

type* min_number(const type* left, const type* right)
{
    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "MIN_NUMBER: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "MIN_NUMBER: right must be a number");

    return  (type*) (((int) left->data) < ((int) right->data) ? left : right);
}

type* 
add_number(const type* left, const type* right)
{
    type* result = mk_unasigned_number();

    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "ADD_NUMBER: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "ADD_NUMBER: right must be a number");

    result->data = (void*) (((int) left->data) + ((int) right->data));
    
    return result;
}

type* 
mul_number(const type* left, const type* right)
{
    type* result = mk_unasigned_number();

    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "MUL_NUMBER: left must be a number");
    assert_throw(is_number(right),
	   TYPE_ERROR,
	   "MUL_NUMBER: right must be a number");

    result->data = (void*) (((int) left->data) * ((int) right->data));
    
    return result;
}

static type* 
_sub_two_numbers(const type* left, const type* right)
{
    type* result = mk_unasigned_number();

    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "SUB_NUMBER: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "SUB_NUMBER: right must be a number");

    result->data = (void*) ((int) left->data - (int) right->data);
    
    return result;
}

static type* 
_sub_numbers(const type* number, const type* numbers)
{
    type* result = nil();

    if (is_nil(numbers))
    {
        result = (type*) number;
    }
    else
    {
        result = _sub_numbers(_sub_two_numbers(number, car(numbers)), 
                              cdr(numbers));
    }

    return result;
}

type* 
sub_numbers(const type* numbers)
{
    type* result = nil();

    assert(!is_nil(numbers) && "SUB_NUMBERS: list of numbers can not be nil");

    if (length(numbers) == 1)
    {
        result = _sub_two_numbers(mk_number("0", 1, TRUE), car(numbers));
    }
    else
    {
        result = _sub_numbers(car(numbers), cdr(numbers));
    }
    
    assert(!is_nil(result) && "SUB_NUMBERS: wrong result");
    
    return result;
}

static type* 
_div_two_numbers(const type* left, const type* right)
{
    type* result = mk_unasigned_number();

    assert_throw(is_number(left),
		 TYPE_ERROR,
		 "DIV_NUMBER: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "DIV_NUMBER: right must be a number");

    result->data = (void*) ((int) left->data / (int) right->data);
    
    return result;
}

static type* 
_div_numbers(const type* number, const type* numbers)
{
    type* result = nil();

    if (is_nil(numbers))
    {
        result = (type*) number;
    }
    else
    {
        result = _div_numbers(_div_two_numbers(number, car(numbers)), 
                              cdr(numbers));
    }

    return result;
}

type* 
div_numbers(const type* numbers)
{
    type* result = nil();

    assert(!is_nil(numbers) && "DIV_NUMBERS: list of numbers can not be nil");

    if (length(numbers) == 1)
    {
        result = _div_two_numbers(mk_number("1", 1, TRUE), car(numbers));
    }
    else
    {
        result = _div_numbers(car(numbers), cdr(numbers));
    }
    
    assert(!is_nil(result) && "DIV_NUMBERS: wrong result");
    
    return result;
}


type* 
remainder_number(const type* left, const type* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR, 
                 "REMAINDER: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR, 
                 "REMAINDER: right must be a number");

    return mk_number_from_int(
        (int)fmodf((float)((int)left->data), (float)((int)right->data)));
}


type* 
number_to_string(const type* n)
{
    int i;
    char digits[100];
    int number;
    int n_digits;
    int negative = FALSE;
    
    assert_throw(is_number(n),
                 TYPE_ERROR, 
                 "STRING_TO_NUMBER: n must be a string");    

    number = (int)n->data;

    if (number == 0)
    {
        n_digits = 0;
        digits[0] = '0';
    }
    else
    {
        if (number < 0)
        {
            negative = TRUE;
            digits[0] = '-';
            number *= -1;
        }
        
        n_digits = log10(number);
        
        if (negative) n_digits++;
        
        for (i = n_digits; (negative ? i > 0 : i >= 0); i--)
        {
            digits[i] = (number % 10) + '0';
            number /= 10;
        }
    }
    
    return mk_string_with_length(digits, n_digits + 1);
}

unsigned int 
number_hash(const type* number)
{
    /* @todo fix better algorithm */
    return (unsigned int) number->data;
}
