#ifndef _HASH_TABLE_H_
#define _HASH_TABLE_H_

#include "type.h"

#define HASH_TABLE_DEFAULT_SIZE 1024

TYPE* mk_hash_table(int (*equal) (const TYPE* left, const TYPE* right),
                          unsigned int (*hash) (const TYPE* key));

TYPE* hash_table_ref(const TYPE* hash_table, const TYPE* key);
void hash_table_delete(TYPE* hash_table, const TYPE* key);
void hash_table_set(TYPE* hash_table, const TYPE* key, const TYPE* data);

#endif
