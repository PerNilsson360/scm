#ifndef _TYPE_H_
#define _TYPE_H_

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

struct type
{ 
    int type;
    void* data;
};
typedef struct type  type;

struct PAIR_DATA
{
    type* car;
    type* cdr;
};
typedef struct PAIR_DATA  PAIR_DATA;

struct ALGEBRAIC_DATA
{
    int n_items;
    type* type;
    type* tag;                  /* as symbol */
    type** items;
};

struct BOUND_VAR_DATA
{
    type* symbol;
    unsigned int frame_index;         /* frame number */
    unsigned int var_index;           /* index in the frame */
    int is_inproper_list;             /* variable arguments */
};
typedef struct BOUND_VAR_DATA  BOUND_VAR_DATA;

type* mk_bound_var(type* symbol, 
                   unsigned int frame_index, 
                   unsigned int var_index,
                   int is_inproper_list);
int is_bound_var(const type* exp);

/* NONE used as non printable return type similar to void */
type* mk_none();
int is_none(const type* sexp);

/* pair */
type* cons(const type* car, const type*  cdr);
type* car(const type* list);
type* cdr(const type* list);
void set_car(type* list, const type* value);
void set_cdr(type* list, const type* value);

type* is_list(const type* sexp);
int is_pair(const type* pair);
int is_empty_pair(const type* sexp);
unsigned int length(const type* pair);

int is_eq(const type* left, const type* right);
int is_eqv(const type* left, const type* right);

/* quoted */
type* mk_quoted(const type* sexp);
int is_quoted(const type* sexp);
type* quotation_value(const type* sexp);

/* boolean */
type* mk_boolean(int t);
int is_boolean(const type* sexp);
type* not(const type* sexp);
int is_true(const type* sexp);
#endif
