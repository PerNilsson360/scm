#include <stdlib.h>
#include <string.h>

#include "util.h"
#include "symbol_table.h"

#define DEFAULT_SIZE 4391 /* A prime number */

struct LINK
{
    struct LINK* next;
    const char* symbol;
};
typedef struct LINK  LINK;

static LINK* symbols[DEFAULT_SIZE];

static
LINK*
mk_link(LINK* next, const char* s)
{
    LINK* result = mloc(sizeof(LINK));

    if (result == NULL)
    {
        fprintf(stderr, "MK_LINK: could not allocate memory for link");
        exit(1);
    }

    result->next = next;
    result->symbol = s;

    return result;
}

static
unsigned
int
get_index(const char* s)
{
    unsigned int result = 0;
    for (; *s != 0; s++)
    {
        result = result * 127 + *s;
    }
    return result % DEFAULT_SIZE;
}

void
symbol_table_init()
{
    for (int i = 0; i < DEFAULT_SIZE; i++)
    {
        symbols[i] = NULL;
    }
}

static
LINK*
find_link(LINK* link, const char* s)
{
    while (link != NULL && strcmp(link->symbol, s) != 0)
    {
        link = link->next;
    }
    return link;
}

TYPE*
symbol_table_find(const char* symbol)
{
    TYPE* result;

    unsigned int i = get_index(symbol);
    LINK* slot_link =  symbols[i];
    LINK* link = find_link(slot_link, symbol);
    
    if (link == NULL)
    {
        const char* s = mloc(sizeof(char) * strlen(symbol));
        if (s == NULL)
        {
            fprintf(stderr, "SYMBOL_TABLE_FIND: could not allocate memory for symbol");
            exit(1);
        }
        strcpy((char*)s, symbol);
        symbols[i] = mk_link(slot_link, s);
        result = (TYPE*)(((intptr_t)s) | SYMBOL_TYPE_TAG);
    }
    else
    {
        result = (TYPE*)(((intptr_t)link->symbol) | SYMBOL_TYPE_TAG);
    }
    
    return result;
}
