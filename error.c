#include <string.h>
#include <assert.h>

#include "common.h"
#include "error.h"

void 
assert_throw(int bool, int code, const char* info)
{
    if (!bool)
    {
        throw_error(code, info);
    }
}

void 
throw_error(int code, const char* info)
{
    strncpy(__error_info__, info, ERROR_INFO_LENGTH);
    longjmp(__c_env__, code);
}
