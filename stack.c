#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <gc.h>

#include "type.h"
#include "stack.h"
#include "util.h"

static int max_size = 0;
static int n_pushes = 0;
static STACK stack;

void
stack_init()
{
	stack.size = 1024;
    stack.top = 0;
    stack.data = mloc(sizeof(void*) * stack.size);
}

void 
save(void* data)
{
    if (stack.top == stack.size)
    {
		stack.size = stack.size * 2;
		fprintf(stderr, "STACK: increasing memory to %d * %lu bytes\n",
				stack.size,
				sizeof(void*));
		stack.data = GC_REALLOC(stack.data, sizeof(void*) * stack.size);
    }
    stack.data[stack.top++] = data;
	/* Statisticks */
	++n_pushes;
	if (stack.top > max_size) {
		max_size = stack.top;
	}
}

void 
restore(void** data)
{
    *data = stack.data[--stack.top];
    /* enable garabage collection */
    stack.data[stack.top] = 0;
}

const STACK*
get_stack()
{
	return &stack;
}

void
copy_stack(STACK* dest, const STACK* src)
{
	dest->size = src->size;
	dest->top = src->top;
	dest->data = mloc(sizeof(void*) * src->size);
	memcpy(dest->data, src->data, sizeof(void*) * src->size);
}

void
assign_stack(const STACK* s)
{
	stack.size = s->size;
	stack.top = s->top;
	stack.data = GC_REALLOC(stack.data,  sizeof(void*) * stack.size);
	memcpy(stack.data, s->data,  sizeof(void*) * stack.size);
}

int 
is_empty()
{
    return stack.top == 0;
}

void
stack_print_statistics() {
	fprintf(stderr, "STACK: size: %d pushes: %d max %d\n", stack.top, n_pushes, max_size);
	n_pushes =0;				/* reset statistics */
	max_size = 0;
}
