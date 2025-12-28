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
#include <string.h>
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <gc.h>

#include "type.h"
#include "common.h"
#include "error.h"
#include "io.h"
#include "eval.h" 
#include "env.h"
#include "symbol.h"
#include "str.h"
#include "port.h"
#include "primitive_procedure.h"
#include "stack.h"
#include "read.h"
#include "char.h"
#include "elab.h"
#include "graphics.h"
#include "util.h"

static int script_has_run;

static 
void
populate_initial_environment(int argc, char** argv, TYPE* env)
{
    TYPE* sexp;
    extern const char* prelude;
    FILE* file = fmemopen((void*)prelude, strlen(prelude), "r");
    do
    {
        sexp = read_from_file(file);
        
        if (!is_eof_object(sexp))
        {
            stack_init();
	    sexp = xlat(sexp);
            eval(sexp, env);
        }
    }
    while (!is_eof_object(sexp));
    fclose(file);
}

static
void
interactive(TYPE* env)
{
    TYPE* sexp;
        
    while (TRUE)
    {
        fprintf(stdout, ">");

        if (fflush(NULL) == -1)
        {
            fprintf(stderr, "INTERACTIVE: could not flush buffers.\n");
            exit(1);
        }
        
        stack_init();
        sexp = scm_read();
	sexp = xlat(sexp);
        display(eval(sexp, env), stdout);
    }
}

static
TYPE*
mk_command_line_list(int argc, char**argv)
{
    TYPE* result = nil();
    for (int i = 0; i < argc; i++)
    {
	result = cons(mk_string_with_length(argv[i], strlen(argv[i])), result);
    }
    return reverse(result);
}

static
void
script_mode(int argc, char** argv, TYPE* env)
{
    TYPE* sexp;
    const char* file_name = argv[1];
    TYPE* command_line = mk_command_line_list(argc, argv);
    define_variable(mk_symbol("command-line"), command_line, env);
    
    TYPE* port = open_input_file(mk_string_with_length(file_name,
                                                       strlen(file_name)));
    TYPE* hash = mk_char('#');
    TYPE* newline = mk_char('\n');
    TYPE* c = peek_char_from_port(port);
    
    /* if the first line has # we assume it is a "#!/..." line */
    if (is_char_equal(c, hash))
    {
        do
        {
            c = read_char_from_port(port);
        }
        while (!is_eof_object(c) && !is_char_equal(c, newline));
    }
    
    do
    {
        stack_init();
        sexp = read_from_port(port);
        if (is_eof_object(sexp))
        {
            break;
        }
        sexp = xlat(sexp);
        eval(sexp, env);
    }
    while(TRUE);
}
    
int
main(int argc, char** argv)
{   
    TYPE* env;
    TYPE* environment_symbol;
    script_has_run = FALSE;
    GC_INIT();
    
    init_symbol_table();
    init_primitive_procedures();
    gr_init();
    
    env = get_global_env();
    environment_symbol = mk_symbol("environment");
    define_variable(environment_symbol, env, env);

    /* @todo fix reading of prelude in a more robust way */
    populate_initial_environment(argc, argv, env);
    
    int status = setjmp(__c_env__);
    
    switch(status)
    {
    case SETJMP_INIT:
        /* first time in setjmp */
        break;
    case NO_ERROR:
    case PARSE_ERROR:
    case EVAL_ERROR:
    case APPLY_ERROR:
    case TYPE_ERROR:
    case CONSTRAINT_ERROR:
    case OS_ERROR:
        printf("%s\n", __error_info__);
        break;
    default:
        assert(FALSE && "CATCH_ERROR: not an implemented error");
    }
    
    if (argc < 2)
    {
        interactive(env);
    }
    else
    {
        if (script_has_run)
        {
            fprintf(stderr, "MAIN: error in script\n");
            return 1;
        }
        script_has_run = TRUE;
        script_mode(argc, argv, env);
    }
    
    return 0;
}
