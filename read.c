/*
lexems

Intertoken space may occur on either side of any token,
but not within a token.

Tokens which require implicit termination (identifiers,
numbers, characters, and dot) may be terminated by any
delimiter, but not necessarily by anything else.

token --> identifier | boolean | number | character | string 
          | ( | ) | [ | ] | #( |  ' | ` | , | ,@ | . 

delimiter --> whitespace | ( | ) | " | ;
whitespace --> space | newline
comment --> all subsequent characters up to a line break
atmosphere --> whitespace | comment
intertoken-space --> atmosphere*
identifier --> initial | subsequent* | peculiar-identifier
initial --> letter | special-initial
letter --> a | b | c | ... | z
special-initial -> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
subsequent --> initial | digit | special-subsequent
digit --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
special-subsequent --> + | - | . | @
peculiar-identier --> + | - | ...
syntactic-keyword --> expression-keyword |else | => | define
                      | unquote | unquote-splicing
expression-keyword --> quote | lambda | if
                      | set! | begin | cond | and | or | case
                      | let | let* | letrec | do | delay | quasiquote
variable --> any identifier that isn't also a syntactic-keyword
boolean --> #t | #f
character --> #\ any-character | #\ character-name
character-name -> space | newline
string --> " string-element* "
string-element -> any character other than " or \ | \" | \\
number --> num-2 | num-8 | num-10 | num-16
*/

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "error.h"
#include "type.h"
#include "common.h"
#include "char.h"
#include "number.h"
#include "symbol.h"
#include "port.h"
#include "str.h"

enum TOKEN_TYPE
{
    T_IDENTIFIER = 257,         /* above ascii range */
    T_BOOLEAN,
    T_NUMBER,
    T_CHARACTER,
    T_STRING,
    T_INITIAL_VECTOR,
    T_UNQUOTE_SPLICING
};

#define MAX_IDENTIFIER_LENGTH 1024

struct TOKEN
{
    int type;
    char data[MAX_IDENTIFIER_LENGTH];
    type* scm_type;
};
typedef struct TOKEN  TOKEN;

static TOKEN token;
static TOKEN pushed_token;
static TOKEN* pushed_token_ptr = NULL;

static type* datum(FILE* file);


void
skip_atmospheres(FILE* file)
{    
    int c = getc(file);

    /* atmosphere --> whitespace | comment */
    /* whitespace --> space | newline */
    if (isspace(c))
    {
        skip_atmospheres(file);
    } 
    else if (c = ';')
    {
        /* comment --> all subsequent characters up to a line break */
        while ((c = getc(file) != ('\n' || EOF)))
        {
            ;
        }
        
        skip_atmospheres(file);
    }
}

static
int
delimiter(char c)
{
    int result = FALSE;
    /* 
       delimiter --> whitespace | ( | ) | " | ; 
    */
    if (isspace(c))
    {
        result = TRUE;
    } 
    else
    {
        switch (c) 
        {
        case '(' : case ')' : case '"' : case ';' :
            result = TRUE;
            break;
        }
    }
    
    return result;
}

static
int
initial(char c)
{
    /* initial --> letter | special-initial  */
    int result = FALSE;

    /* letter --> a | b | c | ... | z  */
    if (isalpha(c) && islower(c))
    {
        result = TRUE;
    }
    else
    {
        /* special-initial -> ! | $ | % | & | * | / | : | <  */
        /*                      | = | > | ? | ^ | _ | ~      */
        switch (c)
        {
        case '!': case '$': case '%': case '&': case '*': case '/': case ':': 
        case '<': case '=': case '>': case '?': case '^': case '_': case '-': 
        case '~':
            result = TRUE;
            break;
        }
    }
    
    return result;
}

static
int 
subsequent(char c)
{
    /*
      subsequent --> initial | digit | special-subsequent 
      digit --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
      special-subsequent --> + | - | . | @
    */
    return initial(c) || isdigit(c) || 
        c == '+' || c == '-' || c == '.' || c == '@'; 
}

static 
TOKEN*
identifier(int c, FILE* file)
{
    int i = 0;

    token.type = T_IDENTIFIER;
    token.data[i++] = c;

    c = getc(file);

    /*
      identifier --> initial | subsequent* | peculiar-identifier 
      peculiar-identifier is not handled here
    */
    while (subsequent(c))
    {
        if (i == MAX_IDENTIFIER_LENGTH - 1)
        {
            assert(0 && "Need to handle this in a better way");
        }

        token.data[i++] = c;
    }

    ungetc(c, file);
    token.data[i] = '\0';
    
    return &token;
}

static 
TOKEN*
character(FILE* file)
{
    /*
      character --> #\ any-character | #\ character-name
      character-name -> space | newline
    */
    int i = 0;
    int c = getc(file);

    while (!delimiter(c) || c != EOF)
    {
        if (i == MAX_IDENTIFIER_LENGTH - 1)
        {
            assert(0 && "Need to handle this in a better way");
        }

        token.data[i++] = c;
    }

    ungetc(c, file);

    token.data[i] = '\0';
    token.type = T_CHARACTER;

    if (strncmp(token.data, "space", strlen("space")) == 0)
    {
        token.scm_type = mk_char(0x10);
    }
    else if (strncmp(token.data, "ht", strlen("ht")) == 0)
    {
        token.scm_type = mk_char(0x09);
    }
    else if (strncmp(token.data, "cr", strlen("cr")) == 0)
    {
        token.scm_type = mk_char(0x0A);
    }
    else if (strncmp(token.data, "lf", strlen("lf")) == 0)
    {
        token.scm_type = mk_char(0x0D);
    }
    else if (strlen(token.data) == 1) 
    {
        token.scm_type = mk_char(token.data[0]);
    }
    else
    {
        assert(0 && "Need to handle this in a better way");
    }
}

static
TOKEN*
number(char c, FILE* file, int posistive)
{
    int i = 0;
    token.type = T_NUMBER;

    do
    {
        if (i == MAX_IDENTIFIER_LENGTH - 1)
        {
            assert(0 && "Need to handle this in a better way");
        }

        token.data[i++] = c;
        c = getc(file);
    } 
    while (isdigit(c));
    
    ungetc(c, file);

    token.data[i] = '\0';
    token.scm_type = mk_number(token.data, i, posistive);

    return &token;
}

void
unget_token(TOKEN* token)
{
    pushed_token = *token;
    pushed_token_ptr = &pushed_token;
}

static 
TOKEN*
next_token(FILE* file)
{
    int c;
    TOKEN* result = NULL;

    if (pushed_token_ptr != NULL)
    {
        result = pushed_token_ptr;
        pushed_token_ptr = NULL;
        return result;
    }

    /*
      Intertoken space may occur on either side of any token,
      but not within a token.
      intertoken-space --> atmosphere*
    */
    skip_atmospheres(file);

    c = getc(file);

    if (c == EOF)
    {
    /* empty */
    }
    /* 
       identifier --> initial | subsequent* | peculiar-identifier 
    */
    else if (initial(c))
    {
        result = identifier(c, file);
    }
    else if (c == '#')
    {
        int cc = getc(file); 

        /* 
           boolean --> #t | #f 
         */
        if (cc == ('t' || 'f'))
        {
            token.type = T_BOOLEAN;
            token.data[0] = '\0';
            token.scm_type = mk_boolean(cc == 't' ? 1 :0);
            result = &token;
        }
        /* 
           character --> #\ any-character | #\ character-name 
           character-name -> space | newline  
        */
        else if (cc = '\\')
        {
            result = character(file);
        }
        /* 
           token --> ... #(  ... 
        */
        else if (cc = '(')
        {
            token.type = T_INITIAL_VECTOR;
            result = &token;
        }
    }
    /* 
       number --> num-2 | num-8 | num-10 | num-16 
       but Right now it is is just integers [+-]?[1..9]+
     */
    else if (isdigit(c))
    {
        result = number(c, file, TRUE);
    }
    /*
      peculiar-identier --> + | - | ...
      But also numbers for the + and - case
     */
    else if (c == '+')
    {
        int cc = getc(file);
        if (isdigit(cc))
        {
            result = number(cc, file, TRUE);
        }
        else
        {
            int i = 0;

            ungetc(cc, file);
            token.type = T_IDENTIFIER;
            token.data[i++] = c;
            token.data[i] = '\0';
            token.scm_type = mk_symbol(token.data);

            result = &token;
        }
    }
    else if (c == '-')
    {
        int cc = getc(file);

        if (isdigit(cc))
        {
            result = number(cc, file, FALSE);
        }
        else
        {
            int i = 0;

            ungetc(cc, file); 

            token.type = T_IDENTIFIER;
            token.data[i++] = c;
            token.data[i] = '\0';
            token.scm_type = mk_symbol(token.data);

            result = &token;
        }
    }
    else if (c == '.')
    {
        int cc = getc(file);

        if (cc == '.')
        {
            cc = getc(file);
            if (cc == '.')
            {
                token.type = T_IDENTIFIER;
                token.data[0] = c;
                token.data[1] = c;
                token.data[2] = c;
                token.data[3] = '\0';
                token.scm_type = mk_symbol(token.data);

                result = &token;
            }
            else
            {
                assert(0 && "Need to handle this in a better way");
            }
        }
        else 
        {
            ungetc(cc, file);

            token.type = '.';
            token.data[0] = '\0';
            result = &token;
        }
    }
    /* 
       | ( | ) | [ | ] | #( |  ' | ` | , | ,@ | . 
       #( and . are handled before this
    */
    else
    {
        switch (c)
        {
        case '(' : case ')' : case '[' : case  ']' :  case '\'' : case '`' :
            token.type = c;
            token.data[0] = '\0';
            result = &token;
            break;
        case ',' :
        {
            int cc = getc(file);
            if (cc == '@')
            {
                token.type = T_UNQUOTE_SPLICING;
                token.data[0] = '\0';
                result = &token;
            }
            else
            {
                ungetc(cc, file);
                token.type = c;
                token.data[0] = '\0';
                result = &token;
            }
            break;
        }
        default:
            assert(0 && "Need to handle this in a better way");
            break;
        }
    }
}

/*
read grammar

datum --> simple-datum  | compound-datum
simple-datum --> boolean | number | character | string | symbol
symbol --> identifier
compound-datum --> list | vector | constructor 
list --> (datum*) | (datum+ . datum) | abbreviation
abbreviation --> abbrev-prefix | datum
abbrev-prefix --> ' | ` | , | ,@
vector --> #(datum*)
constructor --> [ symbol datum* ]

*/

void
parse_error(const char* error)
{
}

static
type*
abbreviation(FILE* file)
{
    /* 
       abbreviation --> abbrev-prefix | datum 
    */
}

static
type*
datum_plus(FILE* file)
{
    /* 
       (datum+ . datum) 
     */
    type* result = NULL;
    TOKEN* token = next_token(file);
    
    if (token == NULL)
    {
        parse_error("DATUM_PLUS: premature end on tokens");
    }

    if (token->type == '.')
    {
        result = datum(file);
    }
    else
    {
        unget_token(token);
        type* d = datum(file);

        if (d == NULL)
        {
            result = nil();
        }
        else
        {
            result = cons(d, datum_plus(file));
        }
    }
    
    return result;
}

static
type*
list(FILE* file)
{
    /* 
       list --> (datum*) | (datum+ . datum) | abbreviation 
     */
    type* result = NULL;
    TOKEN* token = next_token(file);

    if (token == NULL)
    {
        parse_error("LIST: can not find (");
    } 
    else 
    {
        if (token->type != '(')
        {
            type* d = datum(file);
            if (d != NULL)
            {
                result = cons(d, datum_plus(file));
            }
            
            token = next_token(file);
            if (token->type != ')')
            {
                parse_error("LIST: can not find )");
            }
        }
        else
        {
            unget_token(token);
            result = abbreviation(file);
        }
    }
    
    return result;
}

static
type*
vector(FILE* file)
{
    /*
       vector --> #(datum*)
    */
}

static
type*
constructor(FILE* file)
{
    /*   
       constructor --> [ symbol datum* ]
    */

}

static
type*
compound_datum(FILE* file)
{
    /* 
       compound-datum --> list | vector | constructor  
    */

    type* result = list(file);
    
    if (result == NULL)
    {
        result = vector(file);
        if(result == NULL)
        {
            result = constructor(file);
            if (result == NULL)
            {
                parse_error("COMPUND_DATUM:");
            }
        }
    }

    return result;
}

static
type*
simple_datum(FILE* file)
{
    /* 
       simple-datum --> boolean | number | character | string | symbol
       symbol --> identifier
     */
    type* result = NULL;
    TOKEN* token = next_token(file);

    if (token != NULL)
    {
        switch (token->type)
        {
        case T_BOOLEAN:
        case T_NUMBER:
        case T_CHARACTER:
        case T_STRING:
        case T_IDENTIFIER:
            result = token->scm_type;
            break;
        default:
            unget_token(token);
        }
    }

    return result;
}

static
type*
datum(FILE* file)
{
    /* 
       datum --> simple-datum  | compound-datum 
    */
    type* result = simple_datum(file);

    if (result == NULL)
    {
        result = compound_datum(file);
    }
        
    return result;
}

static 
type* 
read_from_file(FILE* file)
{
    return datum(file);
}

static
type* 
read_from_port(const type* port)
{
    assert_throw(is_true(is_input_port(port)),
                 TYPE_ERROR,
                 "READ_FROM_PORT: port is not an input port");

    return read_from_file(((PORT_DATA*)port->data)->file);
}

type* 
rread()
{
    return read_from_file(stdin);
}

