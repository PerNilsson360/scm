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
#include <time.h>

#include "error.h"
#include "unix.h"
#include "number.h"


void 
nano_sleep(TYPE* seconds, TYPE* micro_seconds)
{
    assert_throw(is_number(seconds) && as_integer(seconds) >= 0,
                 TYPE_ERROR,
                 "NANO_SLEEP: seconds must be a number >= 0");
    
    assert_throw(is_number(micro_seconds) && as_integer(micro_seconds) >= 0,
                 TYPE_ERROR,
                 "NANO_SLEEP: micro-second must be a number >= 0");
    
    struct timespec sleep_time = 
	{ 
	    as_integer(seconds), 
	    as_integer(micro_seconds)
	};
    
    if (nanosleep(&sleep_time, NULL) == -1)
    {
        perror("failed in nano sleep");
        throw_error(OS_ERROR, "failed in nano sleep");
    }
}
