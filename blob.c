#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

#include <gc.h>

#include "error.h"
#include "util.h"
#include "type.h"
#include "number.h"
#include "symbol.h"
#include "blob.h"

int 
is_blob(const TYPE* sexp)
{
    return IS_STRUCT_OF_TYPE(sexp, BLOB);
}

TYPE* 
mk_blob(const TYPE* k)
{
    TYPE* result = mloc(sizeof(TYPE));
        
    if (result == NULL)
    {
        fprintf(stderr, "MAKE_BLOB: could not allocate memory for type");
        exit(1);
    }

    result->type = BLOB;
    result->d.bl = mloc(sizeof(BLOB_DATA));

    if (result->d.bl == NULL)
    {
        fprintf(stderr, "MAKE_BLOB: could not allocate memory for data");
        exit(1);
    }

    unsigned int length = as_integer(k);

    result->d.bl->data = mloc(sizeof(unsigned char) * length);

    if (result->d.bl->data == NULL)
    {
        fprintf(stderr, "MAKE_BLOB: could not allocate memory for array");
        exit(1);
    }

    result->d.bl->length = length;

    return result;
}

TYPE* 
blob_length(const TYPE* blob)
{
    return mk_number_from_int(blob->d.bl->length);
}

TYPE* 
blob_u8_ref(const TYPE* blob, const TYPE* k)
{
    unsigned int index = as_integer(k);

    if (index >= blob->d.bl->length)
    {
        throw_error(CONSTRAINT_ERROR, "BLOB_U8_REF: k is out of range");
    }

    unsigned char* a = blob->d.bl->data;

    return mk_number_from_int(a[index]);
}

void 
blob_u8_set(const TYPE* blob, const TYPE* k, const TYPE* u8)
{
    unsigned int index = as_integer(k);
    
    if (index >= blob->d.bl->length)
    {
        throw_error(CONSTRAINT_ERROR, "BLOB_U8_REF: k is out of range");
    }

    blob->d.bl->data[index] = u8->d.i;
}

void 
display_blob(const TYPE* blob, FILE* file)
{
    assert(is_blob(blob));

    unsigned int length = blob->d.bl->length;
    unsigned char* a = blob->d.bl->data;

    unsigned int i;

    fprintf(file, "<");

    for(i = 0; i < length; i++)
    {
        if (i == (length -1)) 
        {
            fprintf(file, "%xd", a[i]);
        }
        else 
        {
            fprintf(file, "%xd ", a[i]);
        }
    }

    fprintf(file, ">");
}
