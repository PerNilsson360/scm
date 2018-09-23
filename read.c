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
#include "vector.h"
#include "port.h"
#include "str.h"
#include "util.h"

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
    TYPE* scm_type;
};
typedef struct TOKEN  TOKEN;

static TOKEN token;
static TOKEN pushed_token;
static TOKEN* pushed_token_ptr = NULL;

static void
print_token(TOKEN* token)
{
    if (token == NULL)
    {
	return;
    }
    switch (token->type)
    {
    case T_IDENTIFIER:
	fprintf(stderr, "id: %s\n", token->data);
	break;
    case T_BOOLEAN:
	fprintf(stderr, "bool: %s\n", token->data);
	break;
    case T_NUMBER:
	fprintf(stderr, "num: %s\n", token->data);
	break;
    case T_CHARACTER:
	fprintf(stderr, "char: %s\n", token->data);
	break;
    case T_STRING:
	fprintf(stderr, "str: %s\n", token->data);
	break;
    case T_INITIAL_VECTOR:
	fprintf(stderr, "initial vector\n");
	break;
    case T_UNQUOTE_SPLICING:
	fprintf(stderr, "unquote splicing\n");
	break;
    };
}

static TOKEN* next_token(FILE* file);
static int paren_depth;

static
void
skip_until_paren_depth_zero(FILE* file)
{
    while (paren_depth > 0)
    {
	TOKEN* token = next_token(file);
	if (token == NULL)
	{
	    break;
	}
	
        if (token->type == '(')
        {
            paren_depth++;
        }
        else if (token->type == ')')
        {
            paren_depth--;
        }
    } 
}

void
parse_error(FILE* file, const char* error)
{
    skip_until_paren_depth_zero(file);
    pushed_token_ptr = NULL;
    throw_error(PARSE_ERROR, error);
}

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
    else if (c == ';')
    {
        /* comment --> all subsequent characters up to a line break */
        do 
        {
	    c = getc(file);
	    if (c == '\n')
	    {
		break;
	    }
	    else if (c == EOF)
	    {
		ungetc(c, file); /* need generate a EOF token */
		break;
	    }
        } while (TRUE);
        
        skip_atmospheres(file);
    } else {
	ungetc(c, file);
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
        case '<': case '=': case '>': case '?': case '^': case '_': case '~':
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
    /*
      identifier --> initial | subsequent* | peculiar-identifier 
      peculiar-identifier is not handled here
    */

    int i = 0;

    token.type = T_IDENTIFIER;
    token.data[i++] = c;

    c = getc(file);

    while (subsequent(c))
    {
        if (i == MAX_IDENTIFIER_LENGTH - 1)
        {
            assert(0 && "Need to handle this in a better way");
        }

        token.data[i++] = c;
	c = getc(file);
    }

    ungetc(c, file);
    token.data[i] = '\0';
    token.scm_type = mk_symbol(token.data);
	
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

    while (!delimiter(c) && c != EOF)
    {
        if (i == MAX_IDENTIFIER_LENGTH - 1)
        {
            assert(0 && "Need to handle this in a better way");
        }

        token.data[i++] = c;
	c = getc(file);
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
    return &token;
}

static
TOKEN*
number(char c, FILE* file, int positive)
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
    token.scm_type = mk_number(token.data, i, positive);

    return &token;
}

static
TOKEN*
string(FILE* file)
{
    /* string --> " string-element* " */
    /* string-element -> any character other than " or \ | \" | \\ */
    
    int c;
    int i = 0;
    token.type = T_STRING;
    do
    {
	if (i == MAX_IDENTIFIER_LENGTH - 1)
        {
            assert(0 && "Need to handle this in a better way");
        }
	c = getc(file);
	if (c == '"')
	{
	    break;
	}
	if (c == '\\')
	{
	    token.data[i++] = c;
	    c = getc(file);
	    if (c == '"' || c == '\\')
	    {
		token.data[i++] = c;
	    }
	    else 
	    {
		parse_error(file, "STRING: wrong escapes.");
	    }
	}
	else
	{
	    token.data[i++] = c;
	}
    } while(TRUE);

    token.data[i++] = '\0';
    token.scm_type = mk_string_with_length(token.data, i);
    
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
    int c
	;
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
	token.type = EOF;
	token.data[0] = '\0';
	result = &token;
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
        if (cc == 't' || cc == 'f')
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
        else if (cc == '\\')
        {
            result = character(file);
        }
	else if (cc == '(')
        {
	    paren_depth++;
            token.type = T_INITIAL_VECTOR;
            result = &token;
        }
	else
	{
	    parse_error(file, "NEXT_TOKEN: bad # sequence");
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
    /* string --> " string-element* " */
    /* string-element -> any character other than " or \ | \" | \\ */
    else if (c == '"')
    {
	result = string(file);
    }
    /* 
       | ( | ) | [ | ] | #( |  ' | ` | , | ,@ | . 
       #( and . are handled before this
    */
    else
    {
        switch (c)
        {
        case '(' :
	    paren_depth++;
	    token.type = c;
            token.data[0] = '\0';
            result = &token;
            break;
	case ')' :
	    paren_depth--;
	    token.type = c;
            token.data[0] = '\0';
            result = &token;
            break;
	case '[' : case  ']' :  case '\'' : case '`' :
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
	{
	    char buff[1024];
	    snprintf(buff,
		     1024,
		     "NEXT_TOKEN: unknown char: %c",
		     c);
	    parse_error(file, buff);
            break;
	}
        }
    }

    return result;
}

/*
read grammar

datum --> simple-datum  | compound-datum
simple-datum --> boolean | number | character | string | symbol
symbol --> identifier
compound-datum --> list | vector | constructor 
list --> (datum*) | (datum+ . datum) | abbreviation
abbreviation --> abbrev-prefix datum
abbrev-prefix --> ' | ` | , | ,@
vector --> #(datum*)
constructor --> [ symbol datum* ]

*/

static TYPE* datum(FILE* file);

static
TYPE*
abbreviation(FILE* file)
{
    /* 
       abbreviation --> abbrev-prefix datum 
       abbrev-prefix --> ' | ` | , | ,@
    */
    TYPE* result = NULL;
    TOKEN* token = next_token(file);

    if (token == NULL)
    {
	return NULL;
    }
    
    if (token->type == '\'')
    {
	TYPE* d = datum(file);
	
	if (d == NULL)
	{
	    parse_error(file, "ABBREVIATION: quoted datum is NULL.");
	}
	else if (is_empty_pair(d))
	{
	    result = nil();
	}
	else
	{
	    result = mk_quoted(d);
	}
    }
    else if (token->type == '`')
    {
	TYPE* d = datum(file);
	
	if (d == NULL)
	{
	    parse_error(file, "ABBREVIATION: quasiquoted datum is NULL.");
	}

	result = cons(mk_symbol("quasiquote"), d);
    }
    else if (token->type == ',')
    {
	token = next_token(file);
	
	const char* unquote;
        if (token->type == '@')
        {
            unquote = "unquote-splicing";
        }
        else
        {
            unquote = "unquote";
	    unget_token(token);
        }
	
	TYPE* d = datum(file);

	if (d == NULL)
	{
	    parse_error(file, "ABBREVIATION: unquote datum is NULL.");
	}
	
	result = cons(mk_symbol(unquote), cons(d, nil()));
    }
    else
    {
	unget_token(token);
    }
    
    return result;
}

static
TYPE*
datum_plus(FILE* file)
{
    /* 
       (datum+ . datum)
     */
    TYPE* result = NULL;
    TOKEN* token = next_token(file);
    
    if (token == NULL)
    {
        parse_error(file, "DATUM_PLUS: token null");
    }

    if (token->type == ')')
    {
	unget_token(token);
	result = nil();
    }
    else if (token->type == '.')
    {
        result = datum(file);
	if (result == NULL)
	{
	    parse_error(file, "DATUM_PLUS: . follows by NULL");
	}
	else if (is_nil(result))
	{
	    parse_error(file, "DATUM_PLUS: . follows by '()");
	}
    }
    else
    {
        unget_token(token);
        TYPE* d = datum(file);

        if (d == NULL)
        {
	    parse_error(file, "DATUM_PLUS: datum null");
        }
	result = cons(d, datum_plus(file));

    }
    
    return result;
}

static
TYPE*
list(FILE* file)
{
    /* 
       list --> (datum*) | (datum+ . datum) | abbreviation 
     */
    TYPE* result = NULL;
    TOKEN* token = next_token(file);

    if (token->type == '(')
    {
	TYPE* d = datum(file);
	if (d == NULL)
	{
	    result = cons(nil(), nil());
	}
	else
	{
	    result = cons(d, datum_plus(file));
	}
        
	token = next_token(file);
	if (token->type != ')')
	{
	    parse_error(file, "LIST: can not find )");
	}
    }
    else
    {
	unget_token(token);
	result = abbreviation(file);
    }
    
    return result;
}

static
TYPE*
datum_star(FILE* file)
{
    /* 
       datum*
     */
    TYPE* result = nil();
    TYPE* d = datum(file);
    
    while (d != NULL)
    {
	result = cons(d, result);
	d = datum(file);
    }

    return result;
}

static
TYPE*
parse_vector(FILE* file)
{
    /*
       vector --> #(datum*)
    */
    TYPE* result = NULL;
    TOKEN* token = next_token(file);
    
    if (token->type == T_INITIAL_VECTOR)
    {
	TYPE* l = reverse(datum_star(file));
	result = list_to_vector(l);
	
	token = next_token(file);
	if (token->type != ')')
	{
	    parse_error(file, "VECTOR: missing )");
	}
    }
    else
    {
	unget_token(token);
    }
    
    return result;
}

static
TYPE*
constructor(FILE* file)
{
    /*   
       constructor --> [ symbol datum* ]
    */
    return NULL;
}

static
TYPE*
compound_datum(FILE* file)
{
    /* 
       compound-datum --> list | vector | constructor  
    */

    TYPE* result = list(file);
    
    if (result == NULL)
    {
        result = parse_vector(file);
        if(result == NULL)
        {
            result = constructor(file);
        }
    }

    return result;
}

static
TYPE*
simple_datum(FILE* file)
{
    /* 
       simple-datum --> boolean | number | character | string | symbol
       symbol --> identifier
     */
    TYPE* result = NULL;
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
	case EOF:
	    result = mk_eof();
	    break;
        default:
            unget_token(token);
        }
    }

    return result;
}

static
TYPE*
datum(FILE* file)
{
    /* 
       datum --> simple-datum  | compound-datum 
    */
    TYPE* result;
    
    result = simple_datum(file);
	
    if (result == NULL)
    {
	result = compound_datum(file);
    }

    return result;
}

static 
TYPE* 
read_from_file(FILE* file)
{
    paren_depth = 0;
    TYPE* result;
    do
    {
	result = datum(file);

	if (result == NULL)
	{
	    /* failed to read a datum clean up */
	    pushed_token_ptr = NULL;
	}
	
    } while (result == NULL);
    
    return result;
}

TYPE* 
scm_read()
{
    return read_from_file(stdin);
}

TYPE* 
read_from_port(const TYPE* port)
{
    assert_throw(is_true(is_input_port(port)),
                 TYPE_ERROR,
                 "READ_FROM_PORT: port is not an input port");

    return read_from_file(port->d.po->file);
}

TYPE*
_read_char_from_port(FILE* file, char* c)
{
    TYPE* result;
    
    *c = getc(file);
    
    if (*c == EOF)
    {
	result = mk_eof();
    }
    else
    {
	result = mk_char(*c);
    }

    return result;
}

TYPE*
read_char_from_port(const TYPE* port)
{
    char c;
    FILE* file = port->d.po->file;
    
    return _read_char_from_port(file, &c);
}

TYPE*
peek_char_from_port(const TYPE* port)
{
    char c;
    FILE* file = port->d.po->file;
    TYPE* result =  _read_char_from_port(file, &c);
    ungetc(c, file);
    
    return result;
}
