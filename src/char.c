#include <stdlib.h>
#include <stdio.h>
#include <gc.h>

#include "error.h"
#include "number.h"
#include "char.h"
#include "util.h"

TYPE* 
mk_char(char c)
{
    intptr_t result = MK_TAGGED_POINTER(c, CHAR_TYPE_TAG);
    return (TYPE*)result;
}

int
is_char(const TYPE* sexp)
{
    return IS_TAGGED_POINTER_OF_TYPE(sexp, CHAR_TYPE_TAG);
}

int
is_char_equal(const TYPE* left, const TYPE* right)
{
    assert_throw(is_char(left),
		 TYPE_ERROR,
		 "IS_CHAR_EQUAL: left must be a char");
    assert_throw(is_char(right),
		 TYPE_ERROR,
		 "IS_CHAR_EQUAL: right must be a char");
    return left == right;
}

TYPE* 
char_to_integer(const TYPE* sexp)
{
    assert_throw(is_char(sexp),
                 TYPE_ERROR,
                 "CHAR_TO_INTEGER: argument must be a char");
    return (TYPE*)MK_TAGGED_POINTER(GET_INTEGER_FROM_TAG(sexp), INTEGER_TYPE_TAG);
}

TYPE* integer_to_char(const TYPE* sexp)
{
  assert_throw(is_number(sexp),	/* TODO numbers are integers now */
	       TYPE_ERROR,
	       "CHAR_TO_INTEGER: argument must be a integer");
  int i = GET_INTEGER_FROM_TAG(sexp);
  assert_throw(i >= 0,
	       TYPE_ERROR,
	       "CHAR_TO_INTEGER: argument < 0");
  assert_throw(i <= 255,
	       TYPE_ERROR,
	       "CHAR_TO_INTEGER: argument > 255");
  intptr_t result = MK_TAGGED_POINTER(i, CHAR_TYPE_TAG);
  return (TYPE*) result;
}
