#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <gc.h>

#include "type.h"
#include "stack.h"

static int size = 1024;
static void** stack;
/* top points to first free location */
static int top;

void
stack_init()
{
    top = 0;
    stack = GC_MALLOC(sizeof(void*) * size);
}

void 
save(void* data)
{
    if (top == size)
    {
	size = size * 2;
	fprintf(stderr, "STACK: increasing memory to %d\n", size);
	stack = GC_REALLOC(stack, sizeof(TYPE) * size);
    }

    stack[top++] = data;
}

void 
restore(void** data)
{
    assert(top > 0 && top <= size);

    *data = stack[--top];
    /* enable garabage collection */
    stack[top] = 0;
}

int 
is_empty()
{
    return top == 0;
}

