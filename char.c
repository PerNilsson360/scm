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
    TYPE* result = mloc(sizeof(TYPE));

    if (result == NULL)
    {
        fprintf(stderr, "MK_CHAR: could not allocate memory for type");
        exit(1);
    }

    result->type = CHAR;
    
    result->d.i = c;
        
    return result;
}

int
is_char(const TYPE* sexp)
{
    return sexp->type == CHAR;
}

TYPE* 
char_to_integer(const TYPE* sexp)
{
    assert_throw(is_char(sexp),
                 TYPE_ERROR,
                 "CHAR_TO_INTEGER: argument must be a char");
    return mk_number_from_int(sexp->d.i);
}
