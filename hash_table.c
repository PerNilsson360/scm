#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <gc.h>

#include "symbol.h"
#include "number.h"
#include "vector.h"
#include "hash_table.h"

type* 
mk_hash_table(int (*equal) (const type* left, const type* right),
              unsigned int (*hash) (const type* key))
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_HASH_TABLE: could not allocate memory for type");
        exit(1);
    }

    result->type = HASH_TABLE;

    result->data = mloc(sizeof(HASH_TABLE_DATA));
    
    if (result->data == NULL)
    {
        fprintf(stderr, 
                "MK_HASH_TABLE: could not allocate memory for HASH_TABLE_DATA");
        exit(1);
    }

    ((HASH_TABLE_DATA*)result->data)->equal = equal;
    ((HASH_TABLE_DATA*)result->data)->hash = hash;
    ((HASH_TABLE_DATA*)result->data)->vector_size = HASH_TABLE_DEFAULT_SIZE;
    ((HASH_TABLE_DATA*)result->data)->vector = 
        mk_vector(mk_number_from_int(HASH_TABLE_DEFAULT_SIZE), nil());
    
    return result;
}

unsigned int
get_index(const type* hash_table, const type* key)
{
    return 
        ((HASH_TABLE_DATA*)hash_table->data)->hash(key) % 
        ((HASH_TABLE_DATA*)hash_table->data)->vector_size;
}

static type* 
_find(type* list, 
      const type* key, 
      int (*equal)(const type* left, const type* right))
{
    type* result;
    /* @todo remove recursion on this function */
    if (is_nil(list))
    {
        result = list;
    }
    else 
    {
        type* first = car(list);

        if (equal(car(first), key))
        {
            result = cdr(first);
        }
        else
        {
            result = _find(cdr(list), key, equal);
        }   
    }

    return result;
}

static type*
_remove(type* list, 
        const type* key, 
        int (*equal)(const type* left, const type* right))
{
    type* result;

    assert(key != NULL && "_REMOVE: key can not be NULL");
    
    if (is_nil(list))
    {
        result = list;
    }
    else
    {
        type* first = car(list);

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

type* 
hash_table_ref(const type* hash_table, const type* key)
{
    assert(key != NULL && "HASH_TABLE_REF: key can not be NULL");

    return _find(vector_ref(((HASH_TABLE_DATA*)hash_table->data)->vector, 
                            get_index(hash_table, key)),
                 key,
                 ((HASH_TABLE_DATA*)hash_table->data)->equal);
}

void 
hash_table_delete(type* hash_table, const type* key)
{
    assert(key != NULL && "HASH_TABLE_DELETE: key can not be NULL");

    _remove(vector_ref(((HASH_TABLE_DATA*)hash_table->data)->vector, 
                       get_index(hash_table, key)),
            key,
            ((HASH_TABLE_DATA*)hash_table->data)->equal);
}

void 
hash_table_set(type* hash_table, const type* key, const type* data)
{
    type* entry = hash_table_ref(hash_table, key);

    assert(key != NULL && "HASH_TABLE_SET: key can not be NULL");

    if (is_nil(entry))
    {
        vector_set(((HASH_TABLE_DATA*)hash_table->data)->vector,
                   get_index(hash_table, key),
                   cons(cons(key, data), 
                        vector_ref(((HASH_TABLE_DATA*)hash_table->data)->vector, 
                                   get_index(hash_table, key))));
    }
    else
    {
        set_cdr(entry, data);
    }
}
