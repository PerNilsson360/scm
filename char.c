#include <stdlib.h>
#include <stdio.h>
#include <gc.h>

#include "error.h"
#include "number.h"
#include "char.h"

type* 
mk_char(char c)
{
    type* result = mloc(sizeof(type));

    if (result == NULL)
    {
        fprintf(stderr, "MK_CHAR: could not allocate memory for type");
        exit(1);
    }

    result->type = CHAR;
    
    result->data = (void*) ((int) c);
        
    return result;
}

int
is_char(const type* sexp)
{
    return sexp->type == CHAR;
}

type* 
char_to_integer(const type* sexp)
{
    assert_throw(is_char(sexp),
                 TYPE_ERROR,
                 "CHAR_TO_INTEGER: argument must be a char");
    return mk_number_from_int((int) sexp->data);
}
