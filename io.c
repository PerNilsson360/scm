#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>

#include <gc.h>

#include "error.h"
#include "common.h"
#include "type.h"
#include "number.h"
#include "vector.h"
#include "str.h"
#include "symbol.h"
#include "char.h"
#include "port.h"
#include "io.h"
#include "blob.h"
#include "util.h"

static void display_on_file(const TYPE* sexp, FILE* file);
static void display_pair(const TYPE* sexp, FILE* file);
static void display_symbol(const TYPE* sexp, FILE* file);
static void display_number(const TYPE* sexp, FILE* file);
static void display_char(const TYPE* sexp, FILE* file);
static void display_boolean(const TYPE* sexp, FILE* file);
static void display_vector(const TYPE* sexp, int i, FILE* file);
static void display_string(const TYPE* string, FILE* file);
static void display_bound_var(const TYPE* sexp, FILE* file);

static void
display_inside_list(const TYPE* sexp, FILE* file)
{
    if (sexp == NULL)
    {
        fprintf(file, "nil");
    }
    else
    {
        switch(sexp->type)
        {
        case PAIR:
            display_pair(sexp, file);
            break;
        case SYMBOL:
            display_symbol(sexp, file);
            break;
        case INTEGER:
		case RATIONAL:
		case REAL:
		case COMPLEX:
            display_number(sexp, file);
            break;
        case CHAR:
            display_char(sexp, file);
            break;
        case BOOLEAN:
            display_boolean(sexp, file);
            break;
        case VECTOR:
            fprintf(file, "#(");
            display_vector(sexp, 0, file);
            fprintf(file, ")");
            break;
        case STRING:
            display_string(sexp, file);
            break;
        case BLOB:
            display_blob(sexp, file);
            break;
        case IMMUTABLE_STRING:
            display_string(sexp, file);
            break;
        case PROCEDURE:
            fprintf(file, "procedure");
            break;
        case PRIMITIVE_PROCEDURE:
            fprintf(file, "primitive-procedur");
            break;
        case PORT:
            fprintf(file, "port");
            break;
        case ENVIRONMENT:
            fprintf(file, "environment");
            break;
        case ENDOFFILE:
            fprintf(file, "end-of-file");
            break;
        case UDP_SOCKET:
            fprintf(file, "udp-socket");
            break;
        case BOUND_VAR:
            display_bound_var(sexp, file);
            break;
		case ESCAPE_PROC:
			fprintf(file, "escape-proc");
			break;
        default:
            fprintf(stderr, "type %d\n", sexp->type);
			assert(FALSE && 
                   "DISPLAY_INSIDE_LIST: not a valid type");
        }
    }
}

static void
display_pair(const TYPE* sexp, FILE* file)
{
    if (is_pair(sexp->d.p->car))
    {
        display_on_file(sexp->d.p->car, file);
    }
    else
    {
        display_inside_list(sexp->d.p->car, file);
    }

    if (!is_pair(cdr(sexp)) && !is_nil(cdr(sexp)))
    {
        fprintf(file, " .");
    }

    if (!is_nil(sexp->d.p->cdr))
    {
        fprintf(file, " ");
        display_inside_list(sexp->d.p->cdr, file);           
    }
}

static void
display_symbol(const TYPE* sexp, FILE* file)
{
    fprintf(file, "%s", sexp->d.s);
}

static void
display_number(const TYPE* sexp, FILE* file)
{
	if (is_real(sexp)) {
		fprintf(file, "%f", sexp->d.d);
	}
	else
	{
		fprintf(file, "%d", sexp->d.i);
	}
}

static void 
display_char(const TYPE* sexp, FILE* file)
{
    switch (sexp->d.i)
    {
    case 0x10:
        fprintf(file, "#\\space");
        break;
    case 0x09:
        fprintf(file, "#ht");
        break;
    case 0x0A:
        fprintf(file, "#\\cr");
        break;
    case 0x0D:
        fprintf(file, "#\\lf");
        break;
    default:
        fprintf(file, "#\\%c", (char)sexp->d.i);
    }
}

static void 
display_boolean(const TYPE* sexp, FILE* file)
{
    assert(is_boolean(sexp) && "DISPLAY_BOOLEAN: Not a boolean sexp");

    if (is_true(sexp))
    {
        fprintf(file, "#t");
    }
    else
    {
        fprintf(file, "#f");
    }
}

static void
display_vector(const TYPE* sexp, int i, FILE* file)
{
    assert(is_vector(sexp) && "DISPLAY_VECTOR: Not a vector sexp");

    if (i < vector_length(sexp)->d.i) 
    {
        if (i > 0)
        {
            fprintf(file, " ");
        }

        display_on_file(sexp->d.v->slots[i], file);
        display_vector(sexp, ++i, file);
    }
}

static void 
display_string(const TYPE* sexp, FILE* file)
{
    assert(is_string(sexp) && "DISPLAY_STRING: Not a string sexp");
    fprintf(file, "\"%s\"", sexp->d.s);
}

static 
void 
display_bound_var(const TYPE* sexp, FILE* file)
{
    fprintf(file, "<");
    display_symbol(sexp->d.b->symbol, file);
    fprintf(file, 
            ":%u:%u:%d>", 
            sexp->d.b->frame_index,
            sexp->d.b->var_index,
	    sexp->d.b->is_inproper_list);
}

static void 
display_on_file(const TYPE* sexp, FILE* file)
{
    if (sexp != NULL)
    {
        if (is_none(sexp))
        {
            /* nothing to display this is regarded as void return value */
        }
        else if (is_pair(sexp))
        {
            fprintf(file, "(");
            display_inside_list(sexp, file);
            fprintf(file, ")");
        }
        else if (is_vector(sexp))
        {
            fprintf(file, "#(");
            display_vector(sexp, 0, file);
            fprintf(file, ")");
        }
        else
        {
            display_inside_list(sexp, file); /* @todo not nessecarily inside list */
        }
    }
    else
    {
        fprintf(file, "nil");
    }

    if (fflush(NULL) == -1)
    {
        fprintf(stderr, "MAIN: could not flush buffers.\n");
        exit(1);
    }
}

void 
display(const TYPE* sexp)
{
    display_on_file(sexp, stdout);
}

void
newline()
{
    printf("\n");

    if (fflush(NULL) == -1)
    {
        fprintf(stderr, "MAIN: could not flush buffers.\n");
        exit(1);
    }

}

void 
error(const TYPE* sexp)
{
    display(sexp);
    assert_throw(FALSE,
                 NO_ERROR,
                 "\nApplication code triggered error.");
}

void 
display_debug(const TYPE* sexp)
{
    printf("Object: ");
    display(sexp);
    newline();
}
