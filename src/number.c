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
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>

#include <gc.h>

#include "io.h"
#include "type.h"
#include "str.h"
#include "symbol.h"
#include "error.h"
#include "common.h"
#include "number.h"
#include "util.h"

TYPE* 
mk_unasigned_number(int type)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_NUMBER: could not allocate memory for type");
        exit(1);
    }
    
    result->type = type;

    return result;
}


TYPE* mk_number_from_int(int n)
{
    // TODO: handle/check that overflow make sense.
    // fprintf(stderr, "mk_number_from_int %d %x", ((unsigned int)n & 0x0000000F), -1);
    // assert_throw((n & 0x0FFFFFFF) <= 0x0EFFFFFF, CONSTRAINT_ERROR, "Integer larger than 54 bits");

    intptr_t result = MK_TAGGED_POINTER(n, INTEGER_TYPE_TAG);
    return (TYPE*)result;
}

static
int hex_to_number(char c) {
    int result;
    switch (c) {
    case 'a':
	result = 10;
	break;
    case 'b':
	result = 11;
	break;
    case 'c':
	result = 12;
	break;
    case 'd':
	result = 13;
	break;
    case 'e':
	result = 14;
	break;
    case 'f':
	result = 15;
	break;
    default:
	result = c -'0';
    }
    return result;
}

TYPE* 
mk_number(const char* symbol, unsigned int length, int positive, int radix)
{
    int number = 0;
    unsigned int i = 0;
        
    for (i = 0; i < length; i++)
    {
	number += hex_to_number(symbol[i]);
      
	if (i < length - 1)
	{
	    number *= radix;
	}
    }

    return mk_number_from_int(positive ? number : (- number));
}

TYPE*
mk_real(const char* symbol, int positive)
{
    TYPE* result = mk_unasigned_number(REAL);
    double d = strtod(symbol, NULL);
    result->d.d = positive ? d : -d;
        
    return result;
}

TYPE* 
mk_hex_number(const char* symbol, unsigned int length)
{
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

    return mk_number_from_int(number);
}

int
is_number(const TYPE* number)
{
    if (IS_TAGGED_POINTER_OF_TYPE(number, INTEGER_TYPE_TAG))
    {
        return TRUE;
    }
    int t = number->type; 
    return  t == RATIONAL || t == REAL || t == COMPLEX;
}

int
is_integer(const TYPE* number)
{
    return IS_TAGGED_POINTER_OF_TYPE(number,  INTEGER_TYPE_TAG);
}

int
is_real(const TYPE* number)
{
    return IS_POINTER_TO_STRUCT_OF_TYPE(number, REAL);
}

static
int
int_from_tagged_pointer(const TYPE* number) {
    int i = (intptr_t)number;
    return (i >> 8);
}

static
double
as_real(const TYPE* number)
{
    if (is_integer(number))
    {
	return int_from_tagged_pointer(number);
    } else if (is_real(number)) {
	return number->d.d;
    }
    else {
	assert_throw(FALSE,
		     TYPE_ERROR,
		     "AS_REAL: not a supported number type");
    }
}

static
int
is_atleast_one(int type, const TYPE* t1, const TYPE* t2) {
    return IS_POINTER_TO_STRUCT_OF_TYPE(t1, type) || IS_POINTER_TO_STRUCT_OF_TYPE(t2, type);
}

int
is_number_equal(const TYPE* left, const TYPE* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "IS_NUMBER_EQUAL: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "IS_NUMBER_EQUAL: right must be a number");
        
    if (is_atleast_one(REAL, left, right))
    {
	return as_real(left) == as_real(right);
    }
    else
    {
	return left == right;
    }
}

int
is_number_lt(const TYPE* left, const TYPE* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "IS_NUMBER_LT: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "IS_NUMBER_LT: right must be a number");

    if (is_atleast_one(REAL, left, right))
    {
	return as_real(left) < as_real(right);
    }
    else
    {
	return int_from_tagged_pointer(left) < int_from_tagged_pointer(right);
    }
}

int
is_number_gt(const TYPE* left, const TYPE* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "IS_NUMBER_GT: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "IS_NUMBER_GT: right must be a number");

    if (is_atleast_one(REAL, left, right))
    {
	return as_real(left) > as_real(right);
    }
    else
    {
	return int_from_tagged_pointer(left) > int_from_tagged_pointer(right);
    }
}

int
is_number_lt_eq(const TYPE* left, const TYPE* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "IS_NUMBER_LT_EQUAL: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "IS_NUMBER_LT_EQUAL: right must be a number");
    if (is_atleast_one(REAL, left, right))
    {
	return as_real(left) <= as_real(right);
    }
    else
    {
	return int_from_tagged_pointer(left) <= int_from_tagged_pointer(right);
    }
}

int
is_number_gt_eq(const TYPE* left, const TYPE* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "IS_NUMBER_GT_EQUAL: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "IS_NUMBER_GT_EQUAL: right must be a number");
        
    if (is_atleast_one(REAL, left, right))
    {
	return as_real(left) >= as_real(right);
    }
    else
    {
	return int_from_tagged_pointer(left) >= int_from_tagged_pointer(right);
    }
}

TYPE* 
is_number_zero(const TYPE* n)
{
    assert_throw(is_number(n),
                 TYPE_ERROR,
                 "IS_NUMBER_ZERO: n must be a number");
    if (is_real(n))
    {
	return mk_boolean(n->d.d == 0);
    }
    else
    {
	return mk_boolean(int_from_tagged_pointer(n) == 0);
    }
}

TYPE* 
is_number_positive(const TYPE* n)
{
    assert_throw(is_number(n),
                 TYPE_ERROR,
                 "IS_NUMBER_POSITIVE: n must be a number");

    if (is_real(n))
    {
	return mk_boolean(n->d.d > 0);
    }
    else
    {
	return mk_boolean(int_from_tagged_pointer(n) > 0);
    }
}

TYPE*
is_number_negative(const TYPE* n)
{
    assert_throw(is_number(n),
                 TYPE_ERROR,
                 "IS_NUMBER_NEGATIVE: n must be a number");

    if (is_real(n))
    {
	return mk_boolean(n->d.d < 0);
    }
    else
    {
	return mk_boolean(int_from_tagged_pointer(n) < 0);
    }
}

TYPE* 
is_number_odd(const TYPE* n)
{
    assert_throw(is_number(n),
                 TYPE_ERROR,
                 "IS_NUMBER_ODD: n must be a number");
        
    if (is_real(n))
    {
	return mk_boolean(FALSE); /* todo maybe this can be improved */
    }
    else
    {
	return mk_boolean((int_from_tagged_pointer(n) % 2) == 1);
    }
}

TYPE* 
is_number_even(const TYPE* n)
{
    assert_throw(is_number(n),
		 TYPE_ERROR,
		 "IS_NUMBER_EVEN: n must be a number");

    if (is_real(n))
    {
	return mk_boolean(FALSE); /* todo maybe this can be improved */
    }
    else
    {
	return mk_boolean((int_from_tagged_pointer(n) % 2) == 0);
    }
}

TYPE* 
max_number(const TYPE* left, const TYPE* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "MAX_NUMBER: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "MAX_NUMBER: right must be a number");
    /* TODO: what about REAL */
    return (TYPE*) (int_from_tagged_pointer(left) > int_from_tagged_pointer(right) ? left : right);
}

TYPE* min_number(const TYPE* left, const TYPE* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "MIN_NUMBER: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "MIN_NUMBER: right must be a number");
        
    if (is_atleast_one(REAL, left, right))
    {
	return  (TYPE*) (as_real(left) < as_real(right) ? left : right);
    }
    else
    {
	return  (TYPE*) (int_from_tagged_pointer(left) < int_from_tagged_pointer(right) ?
                         left :
                         right);
    }
}

TYPE* 
add_number(const TYPE* left, const TYPE* right)
{
    TYPE* result;

    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "ADD_NUMBER: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "ADD_NUMBER: right must be a number");

    if (is_atleast_one(REAL, left, right))
    {
	result = mk_unasigned_number(REAL);
	result->d.d = as_real(left) + as_real(right);
    }
    else
    {
	result = mk_number_from_int (int_from_tagged_pointer(left) +
                                     int_from_tagged_pointer(right));
    }
    
    return result;
}

TYPE* 
mul_number(const TYPE* left, const TYPE* right)
{
    TYPE* result;

    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "MUL_NUMBER: left must be a number");
    assert_throw(is_number(right),
		 TYPE_ERROR,
		 "MUL_NUMBER: right must be a number");

    if (is_atleast_one(REAL, left, right))
    {
	result = mk_unasigned_number(REAL);
	result->d.d = as_real(left) * as_real(right);
    }
    else
    {
        result = mk_number_from_int (int_from_tagged_pointer(left) *
                                     int_from_tagged_pointer(right));
    }

    return result;
}

static TYPE* 
_sub_two_numbers(const TYPE* left, const TYPE* right)
{
    TYPE* result;

    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "SUB_NUMBER: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "SUB_NUMBER: right must be a number");

    if (is_atleast_one(REAL, left, right))
    {
	result = mk_unasigned_number(REAL);
	result->d.d = as_real(left) - as_real(right);
    }
    else
    {
        result = mk_number_from_int (int_from_tagged_pointer(left) -
                                     int_from_tagged_pointer(right));
    }
    
    return result;
}

static TYPE* 
_sub_numbers(const TYPE* number, const TYPE* numbers)
{
    TYPE* result = nil();

    if (IS_NIL(numbers))
    {
        result = (TYPE*) number;
    }
    else
    {
        result = _sub_numbers(_sub_two_numbers(number, car(numbers)), 
                              cdr(numbers));
    }

    return result;
}

TYPE* 
sub_numbers(const TYPE* numbers)
{
    TYPE* result = nil();

    if (length(numbers) == 1)
    {
	result = _sub_two_numbers(mk_number("0", 1, TRUE, 10), car(numbers));
    }
    else
    {
        result = _sub_numbers(car(numbers), cdr(numbers));
    }
    
    return result;
}

static TYPE* 
_div_two_numbers(const TYPE* left, const TYPE* right)
{
    TYPE* result = mk_unasigned_number(REAL);

    assert_throw(is_number(left),
                 TYPE_ERROR,
                 "DIV_NUMBER: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR,
                 "DIV_NUMBER: right must be a number");

    result->d.d = as_real(left) / as_real(right);
    
    return result;
}

static TYPE* 
_div_numbers(const TYPE* number, const TYPE* numbers)
{
    TYPE* result = nil();

    if (IS_NIL(numbers))
    {
        result = (TYPE*) number;
    }
    else
    {
        result = _div_numbers(_div_two_numbers(number, car(numbers)), 
                              cdr(numbers));
    }

    return result;
}

TYPE* 
div_numbers(const TYPE* numbers)
{
    TYPE* result = nil();

    assert(!IS_NIL(numbers) && "DIV_NUMBERS: list of numbers can not be nil");

    if (length(numbers) == 1)
    {
	result = _div_two_numbers(mk_number("1", 1, TRUE, 10), car(numbers));
    }
    else
    {
        result = _div_numbers(car(numbers), cdr(numbers));
    }
    
    assert(!IS_NIL(result) && "DIV_NUMBERS: wrong result");
    
    return result;
}


TYPE* 
remainder_number(const TYPE* left, const TYPE* right)
{
    assert_throw(is_number(left),
                 TYPE_ERROR, 
                 "REMAINDER: left must be a number");
    assert_throw(is_number(right),
                 TYPE_ERROR, 
                 "REMAINDER: right must be a number");
    if (is_atleast_one(REAL, left, right))
    {
	TYPE* result = mk_unasigned_number(REAL);
	result->d.d = fmodf(as_real(left), as_real(right));
	return result;
    }
    else
    {
	return mk_number_from_int(
            (int) fmodf(int_from_tagged_pointer(left), int_from_tagged_pointer(right)));
    }
}

TYPE*
round_number(const TYPE* number)
{
    assert_throw(is_number(number),
                 TYPE_ERROR, 
                 "ROUND: argument must be a number");
  
    if (is_real(number))
    {
	return mk_number_from_int((int) (number->d.d + 0.5));
    }
    else
    {
	return (TYPE*)number;
    }
}

TYPE* 
number_to_string(const TYPE* n)
{
    int i;
    char digits[100];
    int number;
    int n_digits;
    int negative = FALSE;
    
    assert_throw(is_number(n),
                 TYPE_ERROR, 
                 "STRING_TO_NUMBER: n must be a string");    

    /* TODO: waht about reals */
    number = int_from_tagged_pointer(n);

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
number_hash(const TYPE* number)
{
    /* @todo fix better algorithm */
    return as_integer(number);
}

int as_integer(const TYPE* number)
{
    /* TODO: looks fishy with cast to unsigned here */
    if (is_real(number)) {
	return (unsigned int) number->d.d;
    }
    else
    {
	return int_from_tagged_pointer(number);
    }
}
