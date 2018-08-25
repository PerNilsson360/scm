#ifndef _STACK_HH_
#define _STACK_HH_

void stack_init();
void save(void* data);
void restore(void** data);
int is_empty();

#endif
