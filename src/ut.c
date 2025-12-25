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
