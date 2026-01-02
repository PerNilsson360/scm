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
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "error.h"

jmp_buf __c_env__;
char __error_info__[ERROR_INFO_LENGTH];

static
void
format_error(int code, const char* format, va_list args)
{
    vsnprintf(__error_info__, ERROR_INFO_LENGTH, format, args);
}

void 
assert_throw(int bool, int code, const char* format, ...)
{
    if (!bool)
    {
	va_list args;
	va_start(args, format);
        format_error(code, format, args);
	va_end(args);
	longjmp(__c_env__, code);
    }
}

void 
throw_error(int code, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    format_error(code, format, args);
    va_end(args);
    longjmp(__c_env__, code);
}
