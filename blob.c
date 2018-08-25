#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

#include <gc.h>

#include "error.h"
#include "type.h"
#include "number.h"
#include "symbol.h"
#include "blob.h"

int 
is_blob(const type* sexp)
{
    return !is_nil(sexp) && sexp->type == BLOB;
}

type* 
mk_blob(const type* k)
{
    type* result = mloc(sizeof(type));
        
    if (result == NULL)
    {
        fprintf(stderr, "MAKE_BLOB: could not allocate memory for type");
        exit(1);
    }

    result->type = BLOB;
    result->data = mloc(sizeof(PAIR_DATA));

    if (result->data == NULL)
    {
        fprintf(stderr, "MAKE_BLOB: could not allocate memory for data");
        exit(1);
    }

    unsigned int length = (unsigned int)k->data;

   ((PAIR_DATA*) result->data)->car = mloc(sizeof(unsigned char) * length);

    if (((PAIR_DATA*) result->data)->car == NULL)
    {
        fprintf(stderr, "MAKE_BLOB: could not allocate memory for array");
        exit(1);
    }

    ((PAIR_DATA*) result->data)->cdr = (type*)length;

    return result;
}

type* 
blob_length(const type* blob)
{
    return mk_number_from_int((unsigned int)((PAIR_DATA*) blob->data)->cdr);
}

type* 
blob_u8_ref(const type* blob, const type* k)
{
    unsigned int index = (unsigned int) k->data;

    if (index >= (unsigned int)((PAIR_DATA*) blob->data)->cdr)
    {
        throw_error(CONSTRAINT_ERROR, "BLOB_U8_REF: k is out of range");
    }

    unsigned char* a = (unsigned char*)((PAIR_DATA*) blob->data)->car;

    return mk_number_from_int(a[index]);
}

void 
blob_u8_set(const type* blob, const type* k, const type* u8)
{
    unsigned int index = (unsigned int) k->data;
    
    if (index >= (unsigned int)((PAIR_DATA*) blob->data)->cdr)
    {
        throw_error(CONSTRAINT_ERROR, "BLOB_U8_REF: k is out of range");
    }

    int data = (int) u8->data;

    unsigned char* a = (unsigned char*)((PAIR_DATA*) blob->data)->car;

    a[index] = data;
}

void 
display_blob(const type* blob, FILE* file)
{
    assert(is_blob(blob));

    unsigned int length = (unsigned int)((PAIR_DATA*) blob->data)->cdr;
    unsigned char* a = (unsigned char*)((PAIR_DATA*) blob->data)->car;

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
