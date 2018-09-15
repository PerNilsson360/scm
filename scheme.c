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

static 
void
populate_initial_environment(type* env)
{
    type* sexp;
    const char* prelude = 
        "/home/per/prg/c++/scheme/prelude_no_translation.scm";

    type* port = open_input_file(mk_string_with_length(prelude, 
                                                       strlen(prelude)));

    do
    {
        sexp = read_from_port(port);
        
        if (!is_eof_object(sexp))
        {
            stack_init();
            display(eval_no_translation(sexp, env));

            if (fflush(NULL) == -1)
            {
                fprintf(stderr, 
                        "POPULATE_INITIAL_ENVIRONMENT: "
                        "could not flush buffers.\n");
                exit(1);
            }
        }        
    }
    while (!is_eof_object(sexp));

    prelude = "/home/per/prg/c++/scheme/prelude.scm";
    port = open_input_file(mk_string_with_length(prelude, strlen(prelude)));

    do
    {
        sexp = read_from_port(port);
        
        if (!is_eof_object(sexp))
        {
            stack_init();
            display(eval(sexp, env));

            if (fflush(NULL) == -1)
            {
                fprintf(stderr,
                        "POPULATE_INITIAL_ENVIRONMENT: "
                        "could not flush buffers.\n");
                exit(1);
            }
        }
    }
    while (!is_eof_object(sexp));
}

int
main()
{   
    type* env;
    const char* e = "environment";
    type* environment_symbol;
    type* sexp;

    GC_INIT();
    
    init_symbol_table();
    init_primitive_procedures();

    environment_symbol = mk_symbol(e);
    
    env = extend_environment(nil(), nil(), mk_env(nil())); 

    define_variable(environment_symbol, env, env);
    /* @todo fix reading of prelude in a more robust way */
    populate_initial_environment(env);

    switch(setjmp(__c_env__))
    {
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

    while (TRUE)
    {
        fprintf(stdout, ">");

        if (fflush(NULL) == -1)
        {
            fprintf(stderr, "MAIN: could not flush buffers.\n");
            exit(1);
        }
        
        stack_init();
        sexp = rread();
        /* display_debug(sexp); */
        display(eval(sexp, env));

        if (fflush(NULL) == -1)
        {
            fprintf(stderr, "MAIN: could not flush buffers.\n");
            exit(1);
        }
    }

    return 0;
}
