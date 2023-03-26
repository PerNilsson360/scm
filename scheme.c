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

static 
void
populate_initial_environment(int argc, char** argv, TYPE* env)
{
    TYPE* sexp;
    TYPE* port;

    const char* prelude = "/home/per/git/scm/prelude.scm";
    port = open_input_file(mk_string_with_length(prelude,
						 strlen(prelude)));
 
    do
    {
        sexp = read_from_port(port);
        
        if (!is_eof_object(sexp))
        {
            stack_init();
			sexp = xlat(sexp);
            eval(sexp, env);
        }
    }
    while (!is_eof_object(sexp));

    close_input_port(port);
}

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
        display(eval(sexp, env));
    }
}

void
script_mode(int argc, char** argv, TYPE* env)
{
    TYPE* sexp;
    const char* file_name = argv[1];
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
    int script_has_run = FALSE;
    GC_INIT();
    
    init_symbol_table();
    init_primitive_procedures();
    
    env = extend_environment(nil(), nil(), mk_env(nil()));
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
