#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <gc.h>

#include "symbol.h"
#include "error.h"
#include "util.h"
#include "number.h"
#include "string.h"
#include "char.h"

int
is_string(const TYPE* sexp)
{
    return IS_POINTER_TO_STRUCT(sexp) && (sexp->type == STRING || sexp->type == IMMUTABLE_STRING);
}

int
is_mutable_string(const TYPE* sexp)
{
    return IS_POINTER_TO_STRUCT(sexp) && sexp->type == STRING;
}

TYPE* 
mk_string_type(unsigned int size)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_STRING: could not allocate memory for type");
        exit(1);
    }

    result->d.s = mloc(sizeof(char) * (size + 1));

    if (result->d.s == NULL)
    {
        fprintf(stderr, "MK_STRING: could not allocate memory for data");
        exit(1);
    }

    result->d.s[size] = '\0';

    result->type = STRING;

    return result;
}

TYPE* 
mk_string(const TYPE* length, const TYPE* character)
{
    int string_length = length->d.i;
    int i;

    assert_throw(is_number(length),
                 TYPE_ERROR,
                 "MK_STRING: length must be an integer");

    assert_throw(is_char(character),
                 TYPE_ERROR,
                 "MK_STRING: character must be a char");
    
    TYPE* result = mk_string_type(string_length);

    for (i = 0; i < string_length; i++)
    {
        result->d.s[i] = character->d.i;
    }
    
    return result;
}

static TYPE*
_mk_string_from_chars(TYPE* string, const TYPE* char_list, unsigned int i)
{
    TYPE* result = string;

    if (!IS_NIL(char_list))
    {
        assert_throw(is_char(car(char_list)),
                     TYPE_ERROR,
                     "_MK_STRING_FROM_CHARS: list must contain chars");

        string->d.s[i] = car(char_list)->d.i;
        result = _mk_string_from_chars(string, cdr(char_list), ++i);
    }

    return result;
}

TYPE* 
mk_string_from_chars(const TYPE* char_list)
{
    TYPE* result = nil();

    assert_throw(is_list(char_list),
                 TYPE_ERROR,
                 "MK_STRING_FROM_CHARS: char_list must be a list");

    result = _mk_string_from_chars(mk_string_type(length(char_list)),
                                   char_list,
                                   0);

    return result;
}

TYPE* 
mk_string_with_length(const char* string, unsigned int length)
{
    TYPE* result = mk_string_type(length);

    nstrcpy(result->d.s, string, length);

    return result;
}

TYPE* 
string_length(const TYPE* sexp)
{
    assert_throw(is_string(sexp),
                 TYPE_ERROR,
                 "STRING_LENGTH: sexp must be a string");
 
    return mk_number_from_int(strlen(sexp->d.s));
}

TYPE* 
string_ref(const TYPE* sexp, const TYPE* k)
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

    return mk_char(sexp->d.s[k->d.i]);
}

void 
string_set(const TYPE* sexp, const TYPE* k, const TYPE* character)
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

    sexp->d.s[k->d.i] = character->d.i;
}

TYPE* 
string_eq(const TYPE* left, const TYPE* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_EQ: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_EQ: right must be a string");

    return mk_boolean(strcmp(left->d.s, right->d.s) == 0);
}

TYPE* 
string_ci_eq(const TYPE* left, const TYPE* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_CI_EQ: left must be a string");
    
    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_CI_EQ: right must be a string");


    return mk_boolean(strcasecmp(left->d.s, right->d.s) == 0);
}

TYPE* 
string_lt(const TYPE* left, const TYPE* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_LT: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_LT: right must be a string");

    return mk_boolean(strcmp(left->d.s, right->d.s) < 0);
}

TYPE* 
string_gt(const TYPE* left, const TYPE* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_GT: left must be a string");
    
    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_GT: right must be a string");

    return mk_boolean(strcmp(left->d.s, right->d.s) > 0);
}

TYPE* 
string_lt_eq(const TYPE* left, const TYPE* right)
{
    return mk_boolean(is_true(string_lt(left, right)) || 
                      is_true(string_eq(left, right)));
}

TYPE* 
string_gt_eq(const TYPE* left, const TYPE* right)
{
    return mk_boolean(is_true(string_gt(left, right)) || 
                      is_true(string_eq(left, right)));
}

TYPE* 
string_ci_lt(const TYPE* left, const TYPE* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_CI_LT: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_CI_LT: right must be a string");

    return mk_boolean(strcasecmp(left->d.s, right->d.s) < 0);
}

TYPE* 
string_ci_gt(const TYPE* left, const TYPE* right)
{
    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "STRING_CI_GT: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "STRING_CI_GT: right must be a string");

    return mk_boolean(strcasecmp(left->d.s, right->d.s) > 0);
}

TYPE* 
string_ci_lt_eq(const TYPE* left, const TYPE* right)
{
    return mk_boolean(is_true(string_ci_lt(left, right)) || 
                      is_true(string_ci_eq(left, right)));
}

TYPE* 
string_ci_gt_eq(const TYPE* left, const TYPE* right)
{
    return mk_boolean(is_true(string_ci_gt(left, right)) || 
                      is_true(string_ci_eq(left, right)));
}

TYPE* 
substring(const TYPE* sexp, const TYPE* start, const TYPE* end)
{
    int size;
    TYPE* result;
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

    size = end->d.i - start->d.i;

    result = mk_string_type(size);
    
    for (dst = 0, src = start->d.i;
         dst < size;
         dst++, src++)
    {
        assert(src < end->d.i);
        (result->d.s)[dst] = sexp->d.s[src];
    }

    return result;
}

TYPE* 
string_append(const TYPE* left, const TYPE* right)
{
    int left_size;
    int right_size;
    int size;
    TYPE* result;
    int i;
    int dst;

    assert_throw(is_string(left),
                 TYPE_ERROR,
                 "SUBSTRING: left must be a string");

    assert_throw(is_string(right),
                 TYPE_ERROR,
                 "SUBSTRING: right must be a number");

    left_size = string_length(left)->d.i;
    right_size = string_length(right)->d.i;

    size = left_size + right_size;

    result = mk_string_type(size);
    
    for (i = 0; i < left_size; i++)
    {
        result->d.s[i] = left->d.s[i];
    }

    for (dst = 0; i < size; i++, dst++)
    {
        result->d.s[i] = right->d.s[dst];
    }

    return result;
}

TYPE* 
string_to_list(const TYPE* sexp)
{
    assert_throw(is_string(sexp),
                 TYPE_ERROR,
                 "STRING_TO_LIST: argument must be a string");
    
    TYPE* result = nil();
    
    for (int i = string_length(sexp)->d.i - 1; i >= 0; i--)
    {
	result = cons(mk_char(sexp->d.s[i]), result);
    }
    
    return result;
}

TYPE* 
list_to_string(const TYPE* sexp)
{
    assert_throw(is_list(sexp),
		 TYPE_ERROR,
		 "LIST_TO_STRING: argument is not a list");

    TYPE* result;
    
    if (IS_NIL(sexp))
    {
	result = (TYPE*)sexp;
    }
    else
    {
	int i = 0;
	char buffer[MAX_IDENTIFIER_LENGTH];
	const TYPE* list = sexp;
	do
	{
	    const TYPE* c = car(list);
	    
	    assert_throw(is_char(c),
			 TYPE_ERROR,
			 "LIST_TO_STRING: argument must be a char list");
	    assert_throw(i < MAX_IDENTIFIER_LENGTH - 1,
			 CONSTRAINT_ERROR,
			 "LIST_TO_STRING: exceded max identifier length");

	    buffer[i++] = c->d.i;
	    list = cdr(list);
	} while (!IS_NIL(list));
	
	buffer[i] = '\0';
	result = mk_string_with_length(buffer, i);
    }
}

TYPE*
string_to_number(const TYPE* sexp, const TYPE* radix)
{
    /* @todo make this function standard complient */
    char* tail = 0;
    errno = 0;
    
    int i = strtol(sexp->d.s, &tail, radix->d.i);
    
    if (errno != 0)
    {
	perror("STRING_TO_NUMBER:");
	throw_error(CONSTRAINT_ERROR,
		    "STRING_TO_NUMBER:Failed to parse number");
    }

    return mk_number_from_int(i);
}

TYPE* 
string_copy(const TYPE* sexp)
{
    assert(0);
    return 0;
}

/* returns a immutable string where data points to the interned symbol data */
TYPE* 
symbol_to_string(const TYPE* symbol)
{
    TYPE* result;

    assert_throw(is_symbol(symbol),
                 TYPE_ERROR,
                 "SYMBOL_TO_STRING: wrong argument type must be symbol");

    result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_STRING: could not allocate memory for type");
        exit(1);
    }

    result->d.s = symbol->d.s;
    result->type = IMMUTABLE_STRING;

    return result;
}
