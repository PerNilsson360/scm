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
#ifndef _ERROR_H_
#define _ERROR_H_

#include <setjmp.h>

#define SETJMP_INIT       0
#define NO_ERROR          1
#define PARSE_ERROR       2
#define EVAL_ERROR        3
#define APPLY_ERROR       4
#define TYPE_ERROR        5
#define CONSTRAINT_ERROR  6
#define OS_ERROR          7
#define ERROR_INFO_LENGTH 512

extern jmp_buf __c_env__;
extern char __error_info__[ERROR_INFO_LENGTH];
void assert_throw(int bool, int code, const char* info);
void throw_error(int code, const char* info);

#endif
