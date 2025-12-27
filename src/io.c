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
        return;
    }
    
    int type_tag = GET_TYPE_TAG(sexp);
    
    if (type_tag != 0)
    {
        switch(type_tag) {
        case NIL_TYPE_TAG:
            fprintf(file, "'()");
            break;
        case BOOLEAN_TYPE_TAG:
            display_boolean(sexp, file);
            break;
        case INTEGER_TYPE_TAG:
            display_number(sexp, file);
	    break;
	case CHAR_TYPE_TAG:
	    display_char(sexp, file);
	    break;
        }
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
	case RATIONAL:
	case REAL:
	case COMPLEX:
            display_number(sexp, file);
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
	case IF_TYPE:
	    fprintf(file, "(if ");
	    display(IF_PREDICATE(sexp));
	    fprintf(file, " ");
	    display(IF_CONSEQUENT(sexp));
	    fprintf(file, " ");
	    display(IF_ALTERNATIVE(sexp));
	    fprintf(file, ")");
	    break;
	case LAMBDA:
	    fprintf(file, "(lambda ");
	    display(LAMBDA_PARAMETERS(sexp));
	    fprintf(file, " ");
	    display(LAMBDA_BODY(sexp));
	    fprintf(file, ")");
	    break;
	case MATCH:
	    fprintf(file, "(match ");
	    display(MATCH_KEY(sexp));
	    fprintf(file, " ");
	    display(MATCH_CLAUSES(sexp));
	    fprintf(file, ")");
	    break;
	case QUOTE:
	    fprintf(file, "(quote ");
	    display(QUOTATION_VALUE(sexp));
	    fprintf(file, ")");
	    break;
	case DEFINITION:
	    fprintf(file, "(define ");
	    display(DEFINITION_VARIABLE(sexp));
	    fprintf(file, " ");
	    display(DEFINITION_VALUE(sexp));
	    fprintf(file, ")");
	    break;
	case BEGIN_TYPE:
	    fprintf(file, "(begin ");
	    display_pair(BEGIN_ACTIONS(sexp), file);
	    fprintf(file, ")");
	    break;
        default:
            fprintf(stderr, "type %d\n", sexp->type);
	    /*assert(FALSE && 
	      "DISPLAY_INSIDE_LIST: not a valid type");*/
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

    if (!is_pair(cdr(sexp)) && !IS_NIL(cdr(sexp)))
    {
        fprintf(file, " .");
    }

    if (!IS_NIL(sexp->d.p->cdr))
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
	fprintf(file, "%d", as_integer(sexp));
    }
}

static void 
display_char(const TYPE* sexp, FILE* file)
{
    int c = GET_INT_FROM_TYPE_TAGGED_INT(sexp);
    switch (c)
    {
    case 0x10:
        fprintf(file, " ");
        break;
    case 0x09:
        fprintf(file, "\t");
        break;
    case 0x0A:
        fprintf(file, "\n");
        break;
    case 0x0D:
        fprintf(file, "\n");
        break;
    default:
        fprintf(file, "%c", c);
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

    if (i < as_integer(vector_length(sexp))) 
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
