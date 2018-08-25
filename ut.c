#include <stdio.h>

#include "symbol.h"
#include "symbol_ut.h"
#include "env_ut.h"

int
main()
{
    init_symbol_table();

    tst_symbol(); 

    tst_env_lookup_variable_value(); 
    tst_env_extend_environment(); 
    tst_env_set_variable_value(); 
    tst_env_define_variable();

    printf("Unit tests completed\n");
    
    return 0;
}
