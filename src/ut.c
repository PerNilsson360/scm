#include <stdio.h>
#include <assert.h>

#include "symbol.h"
#include "symbol_ut.h"
#include "env_ut.h"
#include "stack.h"
#include "number.h"

void
test_stack()
{
    stack_init();
    
    TYPE* t1;
    for (int j = 0; j < 10; j++)
    {
	for (int i = 0; i < 1000; i++)
	{
	    t1 = mk_number_from_int(i);
	    save(t1);
	}
	
	void* t2;
	
	for (int i = 999; i >= 0; i--)
	{
	    restore(&t2);
	    assert(is_number((TYPE*)t2));
	    TYPE* t3 = mk_number_from_int(i);
	    assert(is_number_equal(t2, t3));
	}
    }
}

int
main()
{
    /* init_symbol_table(); */

    /* tst_symbol();  */

    /* tst_env_lookup_variable_value();  */
    /* tst_env_extend_environment();  */
    /* tst_env_set_variable_value();  */
    /* tst_env_define_variable(); */

    test_stack();
    
    printf("Unit tests completed\n");
    
    return 0;
}
