#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <gc.h>

#include "symbol.h"
#include "error.h"
#include "util.h"
#include "number.h"
#include "string.h"
#include "char.h"

int
is_string(const type* sexp)
{
    return 
        sexp->type == STRING || 
        sexp->type == IMMUTABLE_STRING;
}

int
is_mutable_string(const type* sexp)
{
    return sexp->type == STRING;
}

type* 
mk_string_type(unsigned int size)
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_STRING: could not allocate memory for type");
        exit(1);
    }

    result->data = mloc(sizeof(char) * (size + 1));

    if (result->data == NULL)
    {
        fprintf(stderr, "MK_STRING: could not allocate memory for data");
        exit(1);
    }

    ((char*) result->data)[size] = '\0';

    result->type = STRING;

    return result;
}

type* 
mk_string(const type* length, const type* character)
{
    int string_length = (unsigned int) length->data;
    type* result = mk_string_type(string_length);
    int i;

    assert_throw(is_number(length),
                 TYPE_ERROR,
                 "MK_STRING: length must be an integer");

    assert_throw(is_char(character),
                 TYPE_ERROR,
                 "MK_STRING: character must be a char");


    for (i = 0; i < string_length; i++)
    {
        ((char*) result->data)[i] = (char) (int) character->data;
    }
    
    return result;
}

static type*
_mk_string_from_chars(type* string, const type* char_list, unsigned int i)
{
    type* result = string;

    if (!is_nil(char_list))
    {
        assert_throw(is_char(car(char_list)),
                     TYPE_ERROR,
                     "_MK_STRING_FROM_CHARS: list must contain chars");

        ((char*) string->data)[i] = (char) (int) car(char_list)->data;
        result = _mk_string_from_chars(string, cdr(char_list), ++i);
    }

    return result;
}

type* 
mk_string_from_chars(const type* char_list)
{
    type* result = nil();

    assert_throw(is_true(is_list(char_list)),
                 TYPE_ERROR,
                 "MK_STRING_FROM_CHARS: char_list must be a list");

    result = _mk_string_from_chars(mk_string_type(length(char_list)),
                                   char_list,
                                   0);

    return result;
}

type* 
mk_string_with_length(const char* string, unsigned int length)
{
    type* result = mk_string_type(length);

    nstrcpy(result->data, string, length);

    return result;
}

type* 
string_length(const type* sexp)
{
    assert_throw(is_string(sexp),
                 TYPE_ERROR,
                 "STRING_LENGTH: sexp must be a string");
 
    return mk_number_from_int(strlen((char*) sexp->data));
}

type* 
string_ref(const type* sexp, const type* k)
{
    assert_throw(is_string(sexp),
                 TYPE_ERROR,
                 "STRING_REF: sexp must be a string");

    assert_throw(is_number(k),
                 TYPE_ERROR,
                 "STRING_REF: k must be a number");

    assert_throw(is_number_gt_eq(k, mk_number_from_int(0)) && 
                 is_number_lt(k, string_length(sexp)),
                 TYPE_ERROR,
                 "STRING_REF: 0 >= k < string_length(sexp) does not hold");

    return mk_char(((char*)sexp->data)[(int)k->data]);
}

void 
string_set(const type* sexp, const type* k, const type* character)
{
    assert_throw(is_mutable_string(sexp),
                 TYPE_ERROR,
                 "STRING_SET: sexp must be a mutable string");

    assert_throw(is_number(k),
                 TYPE_ERROR,
                 "STRING_SET: k must be a number");

    assert_throw(is_number_gt_eq(mk_number_from_int(0), k) && 
                 is_number_lt(k, string_length(sexp)),
                 TYPE_ERROR,
                 "STRING_SET: 0 >= k < string_length(sexp) does not hold");

    assert_throw(is_char(character),
                 TYPE_ERROR,
                 "STRING_SET: cgharacter must be a char");

    ((char*) sexp->data)[(int) k->data] = (char) (int) character->data;
}

type* 
string_eq(const type* left, const type* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_EQ: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_EQ: right must be a string");

    return mk_boolean(strcmp((char*) left->data, (char*) right->data) == 0);
}

type* 
string_ci_eq(const type* left, const type* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_CI_EQ: left must be a string");
    
    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_CI_EQ: right must be a string");


    return mk_boolean(strcasecmp((char*) left->data, 
                                 (char*) right->data) == 0);;
}

type* 
string_lt(const type* left, const type* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_LT: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_LT: right must be a string");

    return mk_boolean(strcmp((char*) left->data, (char*) right->data) < 0);
}

type* 
string_gt(const type* left, const type* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_GT: left must be a string");
    
    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_GT: right must be a string");

    return mk_boolean(strcmp((char*) left->data, (char*) right->data) > 0);
}

type* 
string_lt_eq(const type* left, const type* right)
{
    return mk_boolean(is_true(string_lt(left, right)) || 
                      is_true(string_eq(left, right)));
}

type* 
string_gt_eq(const type* left, const type* right)
{
    return mk_boolean(is_true(string_gt(left, right)) || 
                      is_true(string_eq(left, right)));
}

type* 
string_ci_lt(const type* left, const type* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_CI_LT: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_CI_LT: right must be a string");

    return mk_boolean(strcasecmp((char*) left->data, (char*) right->data) < 0);
}

type* 
string_ci_gt(const type* left, const type* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_CI_GT: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_CI_GT: right must be a string");

    return mk_boolean(strcasecmp((char*) left->data, (char*) right->data) > 0);
}

type* 
string_ci_lt_eq(const type* left, const type* right)
{
    return mk_boolean(is_true(string_ci_lt(left, right)) || 
                      is_true(string_ci_eq(left, right)));
}

type* 
string_ci_gt_eq(const type* left, const type* right)
{
    return mk_boolean(is_true(string_ci_gt(left, right)) || 
                      is_true(string_ci_eq(left, right)));
}

type* 
substring(const type* sexp, const type* start, const type* end)
{
    int size;
    type* result;
    int dst; 
    int src;

    assert_throw(is_string(sexp),
                 TYPE_ERROR,
                 "SUBSTRING: sexp must be a string");

    assert_throw(is_number(start),
                 TYPE_ERROR,
                 "SUBSTRING: start must be a number");

    assert_throw(is_number(end),
                 TYPE_ERROR,
                 "SUBSTRING: end must be a number");

    assert_throw(is_number_lt_eq(mk_number_from_int(0) ,start) &&
                 is_number_lt_eq(start, end) &&
                 is_number_lt_eq(end, string_length(sexp)),
                 CONSTRAINT_ERROR,
                 "SUBSTRING: 0 <= start <= end <= string_length(sexp)"
                 " must hold");

    size = (int) end->data - (int) start->data;

    result = mk_string_type(size);
    
    for (dst = 0, src = (int) start->data;
         dst < size;
         dst++, src++)
    {
        assert(src < (int) end->data);
        ((char*) result->data)[dst] = ((char*) sexp->data)[src];
    }

    return result;
}

type* 
string_append(const type* left, const type* right)
{
    int left_size;
    int right_size;
    int size;
    type* result;
    int i;
    int dst;

    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "SUBSTRING: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "SUBSTRING: right must be a number");

    left_size = (int) string_length(left)->data;
    right_size = (int) string_length(right)->data;

    size = left_size + right_size;

    result = mk_string_type(size);
    
    for (i = 0; i < left_size; i++)
    {
        ((char*) result->data)[i] = ((char*) left->data)[i];
    }

    for (dst = 0; i < size; i++, dst++)
    {
        ((char*) result->data)[i] = ((char*) right->data)[dst];
    }

    return result;
}

type* 
string_to_list(const type* sexp)
{
    assert(0);
    return 0;
}

type* 
list_to_string(const type* sexp)
{
    assert(0);
    return 0;
}

type* 
string_copy(const type* sexp)
{
    assert(0);
    return 0;
}

/* returns a immutable string where data points to the interned symbol data */
type* 
symbol_to_string(const type* symbol)
{
    type* result;

    assert_throw(is_symbol(symbol),
                 TYPE_ERROR,
                 "SYMBOL_TO_STRING: wrong argument type must be symbol");

    result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_STRING: could not allocate memory for type");
        exit(1);
    }

    result->data = symbol->data;
    result->type = IMMUTABLE_STRING;

    return result;
}
