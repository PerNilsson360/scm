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

#define MAX_IDENTIFIER_LENGTH 1024

static char read_buffer[MAX_IDENTIFIER_LENGTH];
static int paren_depth;

static type* read_from_file(FILE* file);
static type* read_hex_number(FILE* file, int i);

static void display_on_file(const type* sexp, FILE* file);
static void display_pair(const type* sexp, FILE* file);
static void display_symbol(const type* sexp, FILE* file);
static void display_number(const type* sexp, FILE* file);
static void display_char(const type* sexp, FILE* file);
static void display_boolean(const type* sexp, FILE* file);
static void display_vector(const type* sexp, int i, FILE* file);
static void display_string(const type* string, FILE* file);
static void display_bound_var(const type* sexp, FILE* file);

int
is_eof_object(const type* sexp)
{
    return !is_nil(sexp) && sexp->type == ENDOFFILE;
}

static
type*
mk_eof()
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_VECTOR: could not allocate memory for type");
        exit(1);
    }

    result->type = ENDOFFILE;

    return result;
}

static 
void
save_char(char c, unsigned int i)
{
    read_buffer[i] = c;
}

static 
int 
get_printable_char(FILE* file)
{
    int c = getc(file);
    
    if (!isprint(c) && c != 9 && c != 10 && c != EOF)
    {
        c = get_printable_char(file);
    }

    return c;
}

static
int
initial_identifier_char(char c)
{
    int result = FALSE;

    if (isalpha(c))
    {
        result = TRUE;
    }
    else
    {
        switch (c)
        {
        case '!': case '$': case '%': case '&': case '*': case '/': case ':':
        case '<': case '=': case '>': case '?': case '^': case '_': case '-':  
            result = TRUE;
            break;
        }
    }
    
    return result;
}

static
int 
subsequent_identifier_char(char c)
{
    return initial_identifier_char(c) || isdigit(c) || 
        c == '+' || c == '-' || c == '.' || c == '@'; 
}

static
int hexdigit(char c)
{
    return isdigit(c) || 
        c == 'a' || c == 'b' || c == 'c' || c == 'd' || c == 'e' || c == 'f' ||
        c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F';
}

static
int 
whitespace(char c)
{
    return c == ' ' || c == '\n' || c == '\t';
}

static
void 
skip_whitespace(FILE* file)
{
    int c = get_printable_char(file);

    if (whitespace(c))
    {
        skip_whitespace(file);
    }
    else
    {
        ungetc(c, file);
    }
}

static
void
skip_until_paren_depth_zero(FILE* file)
{
    int c = get_printable_char(file);

    if (paren_depth != 0)
    {
        if (c == '(')
        {
            paren_depth++;
            skip_until_paren_depth_zero(file);
        }
        else if (c == ')')
        {
            paren_depth--;
            skip_until_paren_depth_zero(file);
        }
        else
        {
            skip_until_paren_depth_zero(file);
        }
    } 
}

static 
int
delimiter(char c)
{
    return  whitespace(c) || c == '(' || c == ')' || c == '"' || c == ';';
}

static 
void
match_space(FILE* file)
{
    int c = get_printable_char(file);

    if (!whitespace(c))
    {
         fprintf(stderr, "got char %c\n", c);
         skip_until_paren_depth_zero(file);
         throw_error(TYPE_ERROR,
                     "MATCH-SPACE: could not find matching space");
    }
}

static void
match_right_paren(FILE* file)
{
    int c = get_printable_char(file);
    
    if (c != ')')
    {
        printf("got char %d\n", c);
        skip_until_paren_depth_zero(file);
        throw_error(TYPE_ERROR,
                    "MATCH_RIGHT_PAREN: could not match right paren");
    }
    
    paren_depth--;
}

static type* 
read_list(FILE* file)
{
    type* result;
    type* car = read_from_file(file);
    int c;
    
    skip_whitespace(file);
    c = get_printable_char(file);
    
    if (c == ')')
    {
        paren_depth--;
        result = cons(car, nil());
    }
    else if (c == '.')
    {
        match_space(file);
        result = cons(car, read_from_file(file));
        match_right_paren(file);
    }
    else
    {
        ungetc(c, file);
        result = cons(car, read_list(file));
    }


    return result;
}

static type*
read_char(FILE* file, unsigned int i)
{
    type* result;
    
    int c = get_printable_char(file);
    char result_char = 0;

    if (whitespace(c) && i == 0)
    {
        skip_until_paren_depth_zero(file);
        throw_error(PARSE_ERROR, 
                    "READ: char must be atleast #\\ and an another char");
    }

    if (i == 0)
    {
        save_char((char) c, i);
        result = read_char(file, ++i);
    }
    else if (delimiter(c)) 
    {
        ungetc(c, file);

        if (i == 1)
        {
            result_char = read_buffer[0];
        }
        else
        {
            if (strncmp(read_buffer, "space", strlen("space")) == 0)
            {
                result_char = 0x10;
            }
            else if (strncmp(read_buffer, "ht", strlen("ht")) == 0)
            {
                result_char = 0x09;
            }
            else if (strncmp(read_buffer, "cr", strlen("cr")) == 0)
            {
                result_char = 0x0A;
            }
            else if (strncmp(read_buffer, "lf", strlen("lf")) == 0)
            {
                result_char = 0x0D;
            }
            else
            {
                skip_until_paren_depth_zero(file);
                throw_error(PARSE_ERROR, 
                            "READ: char must be atleast #\\ and an another "
                            "char or one of 'space' 'ht' 'lf'");
            }

        }

        result = mk_char(result_char);            
    }
    else
    {
        save_char((char) c, i);
        result = read_char(file, ++i);
    }
    
    return result;
}

static type* 
read_vector()
{
    assert(FALSE && "READ_VECTOR: Not implemente");
    return NULL;
}


static type* 
read_hash_exp(FILE* file)
{
    type* result = 0;
    
    int c = get_printable_char(file);

    if (c == '\\')
    {
        result = read_char(file, 0);
    }
    else if (c == '(')
    {
        paren_depth++;
        result = list_to_vector(read_list(file));
    }
    else if (c == 'f')
    {
        result = mk_boolean(FALSE);
    }
    else if (c == 't')
    {
        result = mk_boolean(TRUE);
    }
    else if (c == 'x')
    {
        result = read_hex_number(file, 0);
    }
    else
    {
        fprintf(stderr, "READ_HASH_EXP: not a valid character %c", c);
        exit(1);
    }

    return result;
}

static type*
read_identifier(FILE* file, unsigned int i)
{
    type* result = 0;

    int c = get_printable_char(file);
    
    if (subsequent_identifier_char(c))
    {
        save_char((char) c, i);
        result = read_identifier(file, ++i);
    }
    else if (delimiter(c))
    {
        ungetc(c, file);
        read_buffer[i] = '\0';
        result = mk_symbol(read_buffer);
    }

    return result;
}

static type*
read_number(FILE* file, int i)
{
    type* result = 0;
    const char* number;

    int c = get_printable_char(file);
    
    if (isdigit(c))
    {
        save_char((char) c, i);
        result = read_number(file, ++i);
    }
    else if (delimiter(c))
    {
        ungetc(c, file);
        if (read_buffer[0] == '-')
        {
            number = read_buffer;
            number++;
            result = mk_number(number, (i - 1), FALSE);
        }
        else
        {
            result = mk_number(read_buffer, i, TRUE);
        }
    }

    return result;
}

static type*
read_hex_number(FILE* file, int i)
{
    type* result = 0;
    const char* number;

    int c = get_printable_char(file);
    
    if (hexdigit(c))
    {
        save_char((char) c, i);
        result = read_hex_number(file, ++i);
    }
    else if (delimiter(c))
    {
        ungetc(c, file);
        result = mk_hex_number(read_buffer, i);
    }

    return result;
}


static type*
read_string(FILE* file, unsigned int i)
{
    type* result = 0;

    int c1 = getc(file);
    int c2;

    if (c1 == '"')
    {
        result = mk_string_with_length(read_buffer, i);
    }
    else if (c1 == '\\')
    {
        c2 = getc(file);

        if (c2 == '"' || c2 == '\\')
        {
            save_char((char) c1, i);
            save_char((char) c2, ++i);
            result = read_string(file, ++i);
        }
        else
        {
            fprintf(stderr, "READ_STIRNG: bad char after \\");
            exit(1);
        }
    }
    else if (c1 == EOF)
    {
        fprintf(stderr, "READ_STIRNG: unexpected EOF");
        exit(1);
    }
    else 
    {
        save_char((char) c1, i);
        result = read_string(file, ++i);
    }

    return result;
}

static 
void
read_until_end_of_line(FILE* file)
{
    int c = get_printable_char(file);

    if (c != 0x0A )
    {
        read_until_end_of_line(file);
    }
    else if (c == EOF)
    {
        ungetc(c, file);
    }
}

static 
type*
read_from_file(FILE* file)
{
    type* result = 0;

    int c = get_printable_char(file);

    if (c == EOF)
    {
        result = mk_eof();
    }
    else if (c == '(')
    {
        skip_whitespace(file);
        
        c = get_printable_char(file);
        
        if (c == ')')
        {
            result = nil();
        }
        else
        {
            ungetc(c, file);
            paren_depth++;
            result = read_list(file);
        }
    }
    else if (c == '#')
    {
        result = read_hash_exp(file);
    }
    else if (c == '\'')
    {
        type* sexp = read_from_file(file);

        if (is_nil(sexp))
        {
            result = sexp;
        }
        else
        {
            result = mk_quoted(sexp);
        }
    }
    else if (c == '`')
    {
        type* sexp = read_from_file(file);

        if (is_nil(sexp))
        {
            result = sexp;
        }
        else
        {
            const char* qq = "quasiquote";
            result = cons(mk_symbol(qq), sexp);
        }
    }
    else if (c == ',')
    {
        int c = get_printable_char(file);
        const char* quotetag;
        type* sexp;

        if (c == '@')
        {
            quotetag = "unquote-splicing";
        }
        else
        {
            quotetag = "unquote";
            ungetc(c, file);
        }

        sexp = read_from_file(file);

        /* if (is_empty_pair(sexp)) */
        /*       { */
        /*     result = nil(); */
        /* } */
        /* else */
        /* { */
        result = cons(mk_symbol(quotetag), cons(sexp, nil()));
    /* } */
    }
    else if (isdigit(c))
    {
        save_char(c, 0);
        result = read_number(file, 1);
    }
    else if (c == '-')
    {
        int c2 = get_printable_char(file);
        
        if (isdigit(c2))
        {
            save_char(c, 0);
            save_char(c2, 1);
            result = read_number(file, 2);
        }
        else if (subsequent_identifier_char(c2))
        {
            save_char(c, 0);
            save_char(c2, 1);
            result = read_identifier(file, 2);
        }
        else if (delimiter(c2))
        {
            ungetc(c2, file);
            save_char(c, 0);
            read_buffer[1] = '\0';
            result = mk_symbol(read_buffer);
        }
        else 
        {
            assert(FALSE);
        }
    }
    else if (initial_identifier_char(c))
    {
        save_char(c, 0);
        result = read_identifier(file, 1);
    }
    else if (c == '"')
    {
        result = read_string(file, 0);
    }
    else if (c == '+' || c == '*' || c == '/')
    {
        save_char(c, 0);
        read_buffer[1] = '\0'; 
        result = mk_symbol(read_buffer);
    }
    else if (c == ';')
    {
        read_until_end_of_line(file);
        result = read_from_file(file);
    }
    else if (whitespace(c))
    {
        result = read_from_file(file);
    }
    else
    {
        printf("got char %c\n", c);
        skip_until_paren_depth_zero(file);
        throw_error(TYPE_ERROR, "READ: not an inital character %c");
    }

    return result; 
}

type* 
read_from_port(const type* port)
{
    assert_throw(is_true(is_input_port(port)),
                 TYPE_ERROR,
                 "READ_FROM_PORT: port is not an input port");

    paren_depth = 0;
    return read_from_file(((PORT_DATA*)port->data)->file);
}

type* 
read_char_from_port(const type* port)
{
    type* result;
    int c;

    assert_throw(is_true(is_input_port(port)),
                 TYPE_ERROR,
                 "READ_CHAR_FROM_PORT: port is not an input port");

    c = getc(((PORT_DATA*)port->data)->file);

    if (c == EOF) 
    {
        result = mk_eof();
    }
    else 
    {
        result = mk_char(c);
    }
    
    return result;
}

type* 
peek_char_from_port(const type* port)
{
    type* result;
    FILE* file;
    int c;

    assert_throw(is_true(is_input_port(port)),
                 TYPE_ERROR,
                 "READ_CHAR_FROM_PORT: port is not an input port");

    file = ((PORT_DATA*)port->data)->file;

    c = getc(file);
    ungetc(c, file);

    if (c == EOF) 
    {
        result = mk_eof();
    }
    else 
    {
        result = mk_char(c);
    }
    
    return result;
}

type* 
read()
{
    paren_depth = 0;
    return read_from_file(stdin);
}

static void
display_inside_list(const type* sexp, FILE* file)
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
        case NUMBER:
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
        default:
            fprintf(stderr, "type %d\n", sexp->type);
            assert(FALSE && 
                   "DISPLAY_INSIDE_LIST: not a valid type");
        }
    }
}

static void
display_pair(const type* sexp, FILE* file)
{
    if (is_pair(((PAIR_DATA*)sexp->data)->car))
    {
        display_on_file(((PAIR_DATA*)sexp->data)->car, file);
    }
    else
    {
        display_inside_list(((PAIR_DATA*)sexp->data)->car, file);
    }

    if (!is_pair(cdr(sexp)) && !is_nil(cdr(sexp)))
    {
        fprintf(file, " .");
    }

    if (!is_nil(((PAIR_DATA*)sexp->data)->cdr))
    {
        fprintf(file, " ");
        display_inside_list(((PAIR_DATA*)sexp->data)->cdr, file);           
    }
}

static void
display_symbol(const type* sexp, FILE* file)
{
    fprintf(file, "%s", (char*) sexp->data);
}

static void
display_number(const type* sexp, FILE* file)
{
    fprintf(file, "%d", (int) sexp->data);
}

static void 
display_char(const type* sexp, FILE* file)
{
    switch ((int) sexp->data)
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
        fprintf(file, "#\\%c", sexp->data);
    }
}

static void 
display_boolean(const type* sexp, FILE* file)
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
display_vector(const type* sexp, int i, FILE* file)
{
    assert(is_vector(sexp) && "DISPLAY_VECTOR: Not a vector sexp");

    if (i < (int) vector_length(sexp)->data) 
    {
        if (i > 0)
        {
            fprintf(file, " ");
        }

        display_on_file(((vector*)sexp->data)->slots[i], file);
        display_vector(sexp, ++i, file);
    }
}

static void 
display_string(const type* sexp, FILE* file)
{
    assert(is_string(sexp) && "DISPLAY_STRING: Not a string sexp");
    fprintf(file, "\"%s\"", (char*) sexp->data);
}

static 
void 
display_bound_var(const type* sexp, FILE* file)
{
    fprintf(file, "<");
    display_symbol(((BOUND_VAR_DATA*) sexp->data)->symbol, file);
    fprintf(file, 
            ":%u:%u:%d>", 
            ((BOUND_VAR_DATA*) sexp->data)->frame_index,
            ((BOUND_VAR_DATA*) sexp->data)->var_index,
            ((BOUND_VAR_DATA*) sexp->data)->is_inproper_list);
}

static void 
display_on_file(const type* sexp, FILE* file)
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
display(const type* sexp)
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
error(const type* sexp)
{
    display(sexp);
    assert_throw(FALSE,
                 NO_ERROR,
                 "\nApplication code triggered error.");
}

void 
display_debug(const type* sexp)
{
    printf("Object: ");
    display(sexp);
    newline();
}
