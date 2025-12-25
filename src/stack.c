// MIT license
//
// Copyright 2025 Per Nilsson
///
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
    n_pushes =0;                            /* reset statistics */
    max_size = 0;
}
