#include <stdio.h>
#include <assert.h>

#include "type.h"
#include "stack.h"

static TYPE* stack = 0;
void
stack_init()
{
    stack = 0;
}

void 
save(void* data)
{
    stack = cons(data, stack);
}

void 
restore(void** data)
{
    assert(stack != 0);

    *data = car(stack);
    stack = cdr(stack);
}

int 
is_empty()
{
    return stack == 0;
}

