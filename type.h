#ifndef _TYPE_H_
#define _TYPE_H_

#include <stdio.h>
#include <stdint.h>
#include "stack.h"

/**
 * Classical lisp implementation of tagged pointers.
 * It works since malloc returns 16 byte alligned pointers.
 */

#define NIL_TYPE_TAG         1
#define BOOLEAN_TYPE_TAG     2
#define INTEGER_TYPE_TAG     3     
#define BOOLEAN_TRUE_VALUE   0x10

#define GET_TYPE_TAG(POINTER) (((intptr_t)POINTER) & 0xF)
#define IS_TAGGED_POINTER_OF_TYPE(POINTER, TAG) (GET_TYPE_TAG(POINTER) == TAG)
#define IS_TYPE_TAGGED_POINTER(POINTER) (GET_TYPE_TAG(POINTER) != 0)
#define MK_TAGGED_POINTER(VALUE, TAG) ((TYPE*)((VALUE << 8) | TAG))

#define IS_POINTER_TO_STRUCT(POINTER) (GET_TYPE_TAG(POINTER) == 0)
#define IS_POINTER_TO_STRUCT_OF_TYPE(POINTER, TAG) (IS_POINTER_TO_STRUCT(POINTER) && \
                                                    ((const TYPE*)POINTER)->type == TAG)

#define NONE                 0
#define PAIR                 1
#define SYMBOL               2
#define RATIONAL             4
#define INTEGER              3
#define REAL                 5
#define COMPLEX              6
#define CHAR                 7
#define VECTOR               9
#define HASH_TABLE          10
#define STRING              11
#define IMMUTABLE_STRING    12
#define BLOB                13
#define PROCEDURE           14
#define PORT                15
#define ENDOFFILE           16
#define PRIMITIVE_PROCEDURE 17 
#define ENVIRONMENT         18
#define UDP_SOCKET          19
#define TCP_SOCKET          20
#define SERVER_SOCKET       21
#define BOUND_VAR           22
#define ESCAPE_PROC         23
#define IF_TYPE             24
#define LAMBDA              25
#define QUOTE               26
#define ASSIGNMENT          27
#define DEFINITION          28
#define BEGIN_TYPE          29
#define NIL                 30
#define DELAY               31
#define CALL_CC             32
#define MATCH               33
#define APPLY               34

/**
 * TODO: comparing the string representation looks wrong for struct types.
 */
#define is_eq(LEFT, RIGHT) (IS_POINTER_TO_STRUCT(LEFT) && IS_POINTER_TO_STRUCT(RIGHT) ? \
                            (LEFT)->d.s == (RIGHT)->d.s : \
                            LEFT == RIGHT)

struct PAIR_DATA;
struct PROCEDURE_DATA;
struct IF_DATA;
struct VECTOR_DATA;
struct BOUND_VAR_DATA;
struct PORT_DATA;
struct FUNCTION;
struct HASH_TABLE_DATA;
struct BLOB_DATA;

struct TYPE
{ 
    int type;
    union
    {
		int                      i;
		double                   d;
		char*                    s;
		struct TYPE*             t;
		struct ENVIRONMENT_DATA* en;
		struct PAIR_DATA*        p;
		struct PROCEDURE_DATA*   pr;
        struct APPLY_DATA*   a;
		struct IF_DATA*          ifd;
		struct VECTOR_DATA*      v;
		struct BOUND_VAR_DATA*   b;
		struct PORT_DATA*        po;
		struct FUNCTION*         f;
		struct HASH_TABLE_DATA*  h;
		struct BLOB_DATA*        bl;
		struct ESCAPE_PROC_DATA* e;
    } d;
};
typedef struct TYPE  TYPE;

struct PAIR_DATA
{
    TYPE* car;
    TYPE* cdr;
};
typedef struct PAIR_DATA  PAIR_DATA;

struct PROCEDURE_DATA
{
	TYPE* parameters;
    int is_var_args;
    int param_len;
	TYPE* body;
	TYPE* env;

};
typedef struct PROCEDURE_DATA  PROCEDURE_DATA;

struct APPLY_DATA
{
	TYPE* procedure;
    TYPE* args;
    int arg_len;
};
typedef struct APPLY_DATA  APPLY_DATA;


struct IF_DATA
{
	TYPE* predicate;
	TYPE* consequent;
	TYPE* alternative;
};
typedef struct IF_DATA  IF_DATA;

struct ENVIRONMENT_DATA
{
	TYPE* vars;
	TYPE* vals;
	TYPE* previous_frame;
};
typedef struct ENVIRONMENT_DATA ENVIRONMENT_DATA;

struct VECTOR_DATA
{
    struct TYPE* length;
    struct TYPE** slots;
};
typedef struct VECTOR_DATA VECTOR_DATA;

struct BOUND_VAR_DATA
{
    TYPE* symbol;
    unsigned int frame_index;         /* frame number */
    unsigned int var_index;           /* index in the frame */
    int is_inproper_list;             /* variable arguments */
};
typedef struct BOUND_VAR_DATA  BOUND_VAR_DATA;

struct PORT_DATA
{ 
    int read_port;
    FILE* file;
};
typedef struct PORT_DATA  PORT_DATA;

struct FUNCTION
{
    TYPE* (*f) (const TYPE* arguments, const TYPE* env);
};
typedef struct FUNCTION  FUNCTION;

struct HASH_TABLE_DATA
{
    int (*equal)(const TYPE* left, const TYPE* right);
    unsigned int (*hash)(const TYPE* key);
    TYPE* vector;
    int vector_size;
};
typedef struct HASH_TABLE_DATA  HASH_TABLE_DATA;

struct BLOB_DATA
{
    unsigned int length;
    unsigned char* data;
};
typedef struct BLOB_DATA  BLOB_DATA;

/* registers for scheme VM */

struct REGS {
	void* exp;
	void* env;
	void* val;
	void* cont;
	void* proc;
	void* arg1;
	void* unev;
};
typedef struct REGS  REGS;

struct ESCAPE_PROC_DATA {
	STACK stack;
	REGS regs;
};
typedef struct ESCAPE_PROC_DATA  ESCAPE_PROC_DATA;
	
TYPE* mk_escape_proc(const STACK* stack, const REGS* regs);
	
TYPE* mk_bound_var(TYPE* symbol, 
                   unsigned int frame_index, 
                   unsigned int var_index,
                   int is_inproper_list);
int is_bound_var(const TYPE* exp);

/* NONE used as non printable return type similar to void */
TYPE* mk_none();
int is_none(const TYPE* sexp);

/* pair */
TYPE* cons(const TYPE* car, const TYPE*  cdr);
TYPE* car(const TYPE* list);
TYPE* cdr(const TYPE* list);

TYPE* caar(const TYPE* list);
TYPE* cadr(const TYPE* list);
TYPE* caadr(const TYPE* list);
TYPE* caddr(const TYPE* list);
TYPE* cddr(const TYPE* list);
TYPE* cdar(const TYPE* list);
TYPE* cdadr(const TYPE* list);
TYPE* cdddr(const TYPE* list);
TYPE* cadddr(const TYPE* list);

void set_car(TYPE* list, const TYPE* value);
void set_cdr(TYPE* list, const TYPE* value);

int is_list(const TYPE* sexp);
TYPE* list(const TYPE* sexp);
TYPE* mk_list(int elems, ...);
#define is_pair(SEXP) (IS_POINTER_TO_STRUCT_OF_TYPE(SEXP, PAIR))
int is_empty_pair(const TYPE* sexp);
unsigned int length(const TYPE* pair);

int is_procedure(const TYPE* pair);

/* int is_eq(const TYPE* left, const TYPE* right);*/

int is_eqv(const TYPE* left, const TYPE* right);
int is_equal(const TYPE* left, const TYPE* right);

/* quoted */
TYPE* mk_quoted(const TYPE* sexp);
#define IS_QUOTED(TY) (((TYPE*)(TY))->type == QUOTE)
#define QUOTATION_VALUE(TY) (((TYPE*)(TY))->d.t)

/* used for (quote _) */
TYPE* mk_sexp_quoted(const TYPE* sexp);
int is_sexp_quoted(const TYPE* sexp);
TYPE* sexp_quotation_value(const TYPE* sexp);

/* boolean */
TYPE* mk_boolean(int t);
int is_boolean(const TYPE* sexp);
TYPE* not(const TYPE* sexp);
int is_true(const TYPE* sexp);

/* EOF */
int is_eof_object(const TYPE* sexp);
TYPE* mk_eof();

/* internal syntax i.e non list based */
TYPE* mk_assignment(TYPE* variable, TYPE* value);
//#define IS_ASSIGNMENT(TY) (((TYPE*)(TY))->type == ASSIGNMENT)
#define ASSIGNMENT_VARIABLE(TY) (((TYPE*)(TY))->d.p->car)
#define ASSIGNMENT_VALUE(TY) (((TYPE*)(TY))->d.p->cdr)

TYPE* mk_definition(TYPE* variable, TYPE* value);
//#define IS_DEFINITION(TY) (((TYPE*)(TY))->type == DEFINITION)
#define DEFINITION_VARIABLE(TY) (((TYPE*)(TY))->d.p->car)
#define DEFINITION_VALUE(TY) (((TYPE*)(TY))->d.p->cdr)

TYPE* mk_if(TYPE* predicate, TYPE* consequent, TYPE* alternative);
//#define IS_IF(TY) (((TYPE*)(TY))->type == IF_TYPE)
#define IF_PREDICATE(TY) (((TYPE*)(TY))->d.ifd->predicate)
#define IF_CONSEQUENT(TY) (((TYPE*)(TY))->d.ifd->consequent)
#define IF_ALTERNATIVE(TY) (((TYPE*)(TY))->d.ifd->alternative)

TYPE* mk_lambda(TYPE* parametes, TYPE* body);
//#define IS_LAMBDA(TY) (((TYPE*)(TY))->type == LAMBDA)
#define LAMBDA_PARAMETERS(TY) (((TYPE*)(TY))->d.p->car)
#define LAMBDA_BODY(TY) (((TYPE*)(TY))->d.p->cdr)

TYPE* mk_begin(TYPE* actions);
#define BEGIN_ACTIONS(TY) (((TYPE*)(TY))->d.t)

TYPE*
mk_match(TYPE* key, TYPE* clauses);
#define MATCH_KEY(TY) (((TYPE*)(TY))->d.p->car)
#define MATCH_CLAUSES(TY) (((TYPE*)(TY))->d.p->cdr)

TYPE* mk_delay(TYPE* actions);
#define DELAY_ACTIONS(TY) (((TYPE*)(TY))->d.t)

TYPE* mk_call_cc(TYPE* escape_procedure);
#define ESCAPE_PROCEDURE(TY) (((TYPE*)(TY))->d.t)

TYPE* mk_apply(TYPE* procedure, TYPE* arguments);
#define APPLY_PROCEDURE(TY) (((TYPE*)(TY))->d.a->procedure)
#define APPLY_ARGUMENTS(TY) (((TYPE*)(TY))->d.a->args)
#define APPLY_ARG_LEN(TY) (((TYPE*)(TY))->d.a->arg_len)

int is_escape_proc(const TYPE* sexp);
TYPE* mk_escape_proc(const STACK* stack, const REGS* regs);
void copy_regs(REGS* dest, const REGS* src);

#endif
