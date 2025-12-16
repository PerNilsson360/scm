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
