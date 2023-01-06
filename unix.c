#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "error.h"
#include "unix.h"
#include "number.h"


void 
nano_sleep(TYPE* seconds, TYPE* micro_seconds)
{
    assert_throw(is_number(seconds) && seconds->d.i >= 0,
                 TYPE_ERROR,
                 "NANO_SLEEP: seconds must be a number >= 0");
    
    assert_throw(is_number(micro_seconds) && micro_seconds->d.i >= 0,
                 TYPE_ERROR,
                 "NANO_SLEEP: micro-second must be a number >= 0");
    
    struct timespec sleep_time = 
    { 
		seconds->d.i, 
		micro_seconds->d.i
    };
    
    if (nanosleep(&sleep_time, NULL) == -1)
    {
        perror("failed in nano sleep");
        throw_error(OS_ERROR, "failed in nano sleep");
    }
}
