#ifndef _TYPE_H_
#define _TYPE_H_

#include <stdio.h>

#define NONE                 0
#define PAIR                 1
#define SYMBOL               2
#define NUMBER               3
#define CHAR                 4
#define BOOLEAN              5
#define VECTOR               6
#define HASH_TABLE           7
#define STRING               8
#define IMMUTABLE_STRING     9
#define BLOB                10
#define PROCEDURE           11
#define PORT                12
#define ENDOFFILE           13
#define PRIMITIVE_PROCEDURE 14 
#define ENVIRONMENT         15
#define UDP_SOCKET          16
#define TCP_SOCKET          17
#define SERVER_SOCKET       18
#define BOUND_VAR           19
#define ALGEBRAIC           20

struct PAIR_DATA;
struct vector;
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
	int i;
	char* s;
	struct TYPE* t;
	struct PAIR_DATA* p;
	struct vector* v;
	struct BOUND_VAR_DATA* b;
	struct PORT_DATA* po;
	struct FUNCTION* f;
	struct HASH_TABLE_DATA* h;
	struct BLOB_DATA* bl;
    } d;
};
typedef struct TYPE  TYPE;

struct PAIR_DATA
{
    TYPE* car;
    TYPE* cdr;
};
typedef struct PAIR_DATA  PAIR_DATA;

struct vector
{
    struct TYPE* length;
    struct TYPE** slots;
};
typedef struct vector vector;

struct ALGEBRAIC_DATA
{
    int n_items;
    TYPE* type;
    TYPE* tag;                  /* as symbol */
    TYPE** items;
};

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
typedef struct BLOB_DATA BLOB_DATA;


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
void set_car(TYPE* list, const TYPE* value);
void set_cdr(TYPE* list, const TYPE* value);

TYPE* is_list(const TYPE* sexp);
int is_pair(const TYPE* pair);
int is_empty_pair(const TYPE* sexp);
unsigned int length(const TYPE* pair);

int is_eq(const TYPE* left, const TYPE* right);
int is_eqv(const TYPE* left, const TYPE* right);

/* quoted */
TYPE* mk_quoted(const TYPE* sexp);
int is_quoted(const TYPE* sexp);
TYPE* quotation_value(const TYPE* sexp);

/* boolean */
TYPE* mk_boolean(int t);
int is_boolean(const TYPE* sexp);
TYPE* not(const TYPE* sexp);
int is_true(const TYPE* sexp);
#endif
