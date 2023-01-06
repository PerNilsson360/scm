#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>

#include <gc.h>

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
    TYPE* result = mk_unasigned_number(INTEGER);

    result->d.i = n;

    return result;
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
    TYPE* result = mk_unasigned_number(INTEGER);
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

    result->d.i = (positive ? number : (- number));

    return result;
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
    TYPE* result = mk_unasigned_number(INTEGER);
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

    result->d.i = number;

    return result;
}

int
is_number(const TYPE* number)
{
    int t = number->type; 
    return  t == INTEGER || t == RATIONAL || t == REAL || t == COMPLEX;
}

int
is_integer(const TYPE* number)
{
	return number->type == INTEGER;
}

int
is_real(const TYPE* number)
{
	return number->type == REAL;
}

static
double
as_real(const TYPE* number)
{
	if (is_integer(number))
	{
		return  number->d.i;
	} else if (is_real(number)) {
		return  number->d.d;
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
	return t1->type == type || t2->type == type;
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
		return left->d.i == right->d.i;
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
		return left->d.i < right->d.i;
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
		return left->d.i > right->d.i;
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
		return left->d.i <= right->d.i;
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
		return left->d.i >= (int) right->d.i;
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
		return mk_boolean(n->d.i == 0);
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
		return mk_boolean(n->d.i > 0);
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
		return mk_boolean(n->d.i < 0);
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
		return mk_boolean((n->d.i % 2) == 1);
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
	   return mk_boolean(((n->d.i) % 2) == 0);
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

    return (TYPE*) (left->d.i > right->d.i ? left : right);
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
		return  (TYPE*) (left->d.i < right->d.i ? left : right);
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
		result = mk_unasigned_number(INTEGER);
		result->d.i = left->d.i + right->d.i;
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
		result = mk_unasigned_number(INTEGER);
		result->d.i = left->d.i * right->d.i;
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
		result = mk_unasigned_number(INTEGER);
		result->d.i = left->d.i - right->d.i;
	}
    
    return result;
}

static TYPE* 
_sub_numbers(const TYPE* number, const TYPE* numbers)
{
    TYPE* result = nil();

    if (is_nil(numbers))
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

    assert(!is_nil(numbers) && "SUB_NUMBERS: list of numbers can not be nil");

    if (length(numbers) == 1)
    {
      result = _sub_two_numbers(mk_number("0", 1, TRUE, 10), car(numbers));
    }
    else
    {
        result = _sub_numbers(car(numbers), cdr(numbers));
    }
    
    assert(!is_nil(result) && "SUB_NUMBERS: wrong result");
    
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

    if (is_nil(numbers))
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

    assert(!is_nil(numbers) && "DIV_NUMBERS: list of numbers can not be nil");

    if (length(numbers) == 1)
    {
		result = _div_two_numbers(mk_number("1", 1, TRUE, 10), car(numbers));
    }
    else
    {
        result = _div_numbers(car(numbers), cdr(numbers));
    }
    
    assert(!is_nil(result) && "DIV_NUMBERS: wrong result");
    
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
		result->d.d = fmodf(left->d.d, right->d.d);
		return result;
	}
	else
	{
		return mk_number_from_int((int) fmodf(left->d.i, right->d.i));
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
		mk_number_from_int((int) (number->d.d + 0.5));
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

    number = n->d.i;

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
	if (is_real(number)) {
		return (unsigned int) number->d.d;
	}
	else
	{
		return (unsigned int) number->d.i;
	}
}
