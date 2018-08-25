#ifndef _HASH_TABLE_H_
#define _HASH_TABLE_H_

#include "type.h"

#define HASH_TABLE_DEFAULT_SIZE 1024

struct HASH_TABLE_DATA
{
    int (*equal)(const type* left, const type* right);
    unsigned int (*hash)(const type* key);
    type* vector;
    int vector_size;
};
typedef struct HASH_TABLE_DATA  HASH_TABLE_DATA;

type* mk_hash_table(int (*equal) (const type* left, const type* right),
                          unsigned int (*hash) (const type* key));

type* hash_table_ref(const type* hash_table, const type* key);
void hash_table_delete(type* hash_table, const type* key);
void hash_table_set(type* hash_table, const type* key, const type* data);

#endif
