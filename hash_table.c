#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <gc.h>

#include "common.h"
#include "symbol.h"
#include "number.h"
#include "vector.h"
#include "util.h"
#include "hash_table.h"
#include "io.h"

TYPE* 
mk_hash_table(int (*equal) (const TYPE* left, const TYPE* right),
              unsigned int (*hash) (const TYPE* key))
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_HASH_TABLE: could not allocate memory for type");
        exit(1);
    }

    result->type = HASH_TABLE;

    result->d.h = mloc(sizeof(HASH_TABLE_DATA));
    
    if (result->d.h == NULL)
    {
        fprintf(stderr, 
                "MK_HASH_TABLE: could not allocate memory for HASH_TABLE_DATA");
        exit(1);
    }

    result->d.h->equal = equal;
    result->d.h->hash = hash;
    result->d.h->vector_size = HASH_TABLE_DEFAULT_SIZE;
    result->d.h->vector = 
        mk_vector(mk_number_from_int(HASH_TABLE_DEFAULT_SIZE), nil());
    
    return result;
}

unsigned int
get_index(const TYPE* hash_table, const TYPE* key)
{
    return 
        hash_table->d.h->hash(key) % 
        hash_table->d.h->vector_size;
}

static int
_find(TYPE* list, 
      const TYPE* key,
      TYPE** result,
      int (*equal)(const TYPE* left, const TYPE* right))
{
    int found = FALSE;

    while (!IS_NIL(list) && !found)
    {
        TYPE* first = car(list);
        if (equal(car(first), key))
        {
            *result = cdr(first);
            found = TRUE;
        }
        else
        {
            list = cdr(list);
        }
    }
    
    return found;
}

static TYPE*
_remove(TYPE* list, 
        const TYPE* key, 
        int (*equal)(const TYPE* left, const TYPE* right))
{
    TYPE* result;

    assert(key != NULL && "_REMOVE: key can not be NULL");
    
    if (IS_NIL(list))
    {
        result = list;
    }
    else
    {
        TYPE* first = car(list);

        if (equal(car(first), key))
        {
            result = cdr(list);
        }
        else
        {
            result = cons(first, _remove(cdr(list), key, equal));
        }
    }

    return result;
}

int
hash_table_ref(const TYPE* hash_table, const TYPE* key, TYPE** result)
{
    assert(key != NULL && "HASH_TABLE_REF: key can not be NULL");

    return _find(vector_ref(hash_table->d.h->vector, 
                            get_index(hash_table, key)),
                 key,
                 result,
                 hash_table->d.h->equal);
}

void 
hash_table_delete(TYPE* hash_table, const TYPE* key)
{
    assert(key != NULL && "HASH_TABLE_DELETE: key can not be NULL"); 

    _remove(vector_ref(hash_table->d.h->vector, 
                       get_index(hash_table, key)),
            key,
            hash_table->d.h->equal);
}

void 
hash_table_set(TYPE* hash_table, const TYPE* key, const TYPE* data)
{
    TYPE* entry;
    assert(key != NULL && "HASH_TABLE_SET: key can not be NULL");

    int found  = hash_table_ref(hash_table, key, &entry);

    if (found)
    {
        hash_table_delete(hash_table, key);
    }
    
    vector_set(hash_table->d.h->vector,
               get_index(hash_table, key),
               cons(cons(key, data), 
                    vector_ref(hash_table->d.h->vector, 
                               get_index(hash_table, key))));
}
