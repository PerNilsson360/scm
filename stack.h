#ifndef _STACK_HH_
#define _STACK_HH_

struct STACK {
	int size;
	int top; /* top points to first free location */
	void** data;
};
typedef struct STACK  STACK;

void stack_init();
void save(void* data);
void restore(void** data);
const STACK* get_stack();
void copy_stack(STACK* dest, const STACK* src);
void assign_stack(const STACK* s);
int is_empty();

#endif
