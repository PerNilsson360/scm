#include <stdlib.h>
#include <errno.h>
#include <time.h>

#include "error.h"
#include "unix.h"


void 
nano_sleep(type* seconds, type* micro_seconds)
{
    assert_throw(is_number(seconds) && (int)seconds->data >= 0,
                 TYPE_ERROR,
                 "NANO_SLEEP: seconds must be a number >= 0");
    
    assert_throw(is_number(micro_seconds) && (int) micro_seconds->data >= 0,
                 TYPE_ERROR,
                 "NANO_SLEEP: micro-second must be a number >= 0");
    
    struct timespec sleep_time = 
        { 
            (int)seconds->data, 
            (int) micro_seconds->data
        };
    
    if (nanosleep(&sleep_time, NULL) == -1)
    {
        perror("failed in nano sleep");
        throw_error(OS_ERROR, "failed in nano sleep");
    }
}
