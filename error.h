#ifndef _ERROR_H_
#define _ERROR_H_

#include <setjmp.h>

#define NO_ERROR          0
#define PARSE_ERROR       1
#define EVAL_ERROR        2
#define APPLY_ERROR       3
#define TYPE_ERROR        4
#define CONSTRAINT_ERROR  5
#define OS_ERROR          6
#define ERROR_INFO_LENGTH 512

jmp_buf __c_env__;
char __error_info__[ERROR_INFO_LENGTH];
void assert_throw(int bool, int code, const char* info);
void throw_error(int code, const char* info);

#endif
