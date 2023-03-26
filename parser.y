%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "error.h"	
#include "type.h"
#include "symbol.h"
#include "number.h"
#include "port.h"
#include "syntax.h"
#include "io.h"
	
int yylex(); 
int yyerror(const char *p){ fprintf(stderr, "Error %s\n", p); throw_error(PARSE_ERROR, p); }
int get_type_var_num() {
	static int n = 0;
	return n++;
}
TYPE* mk_type_var() {
	return mk_list(2, mk_symbol("var-type"), mk_number_from_int(get_type_var_num()));
}
extern FILE * yyin;
TYPE* syntax_tree;
#define YYDEBUG 1
%}

%token <sval> ID CONST_INT
%token ABSTYPE AND ANDALSO AS CASE DO DATATYPE ELSE END EXCEPTION FN
%token FUN HANDLE IF IN INFIX INFIXR LET OF OP OPEN ORELSE REC THEN
%token SML_TYPE VAL WITH WITHTYPE WHILE
%token ELIPSIS FAT_ARROW THIN_ARROW
%token INT BOOL
%token END_OFF_FILE

%left '-' '+' '*' '/'
%left '='

%start pgm

%union {
    char* sval;
	TYPE* tval;
}

%type <tval> pgm
%type <tval> dec
%type <tval> type
%type <tval> btype
%type <tval> exp
%type <tval> match
%type <tval> pat
%type <tval> apat

%%

 /*
Key words from standard ml definition text.

abstype and andalso as case do datatype else
end exception fn fun handle if in infix
infixr let local nonfix of op open orelse
raise rec then type val with withtype while
( ) [ ] { } , : ; ... _ | = => -> #

[] option i.e 0 or 1 time
{} repetion 0 or many times

BNF Syntax of ML and SML97 Overview
By K.W. Regan—based on syntax diagrams in J.D. Ullman, Elements of ML Programming, ML’97 ed.,
but expanded and regrouped with some fixes. ALL-CAPS are used for nonterminals, and all-lowercase
for literal keywords. Literal ( ) [ ]
are quoted to distinguish them from BNF syntax.
 */

 /*
PGM ::= EXP; | {DEC | MODULE | ;} (Semicolon == "compile me now")
 */

pgm
  : exp ';'                                                                { syntax_tree = $$ = mk_list(2, mk_symbol("exp-prg"), $1); }
  | dec ';'                                                                { syntax_tree = $$ = mk_list(2, mk_symbol("dec-prg"), $1); }
  ;

 /*
DEC ::= val [TVARS] PAT = EXP {and PAT = EXP}
| val rec [TVARS] ID = fn MATCH {and ID = fn MATCH} (SML-NJ; Std
| fun [TVARS] FBIND {and FBIND} ML’97 has "val [TVARS] rec..")
| type TBINDS
| datatype DTBIND {and DTBIND} [withtype TBINDS]
| datatype ID = datatype LID
| abstype DTBIND {and DTBIND} [withtype TBINDS] with {DEC[;]} end
| exception ID [of TYPE | = LID] {and ID [of TYPE | = LID]}
| local {DEC[;]} in {DEC[;]} end
| open LID {LID} dump public structure items into scope
| infix[r] [INT] ID {ID} make [right] assoc. infix fn, prec. INT
| nonfix ID {ID} "nonfix +;" makes you write "+(x,y)".
 */

dec
  : VAL pat '=' exp                                                        { $$ = mk_list(3, mk_symbol("pat-dec"), $2, $4); }
  | VAL REC ID '=' FN match                                                { $$ = mk_list(4, mk_symbol("rec-dec"), mk_symbol($3), mk_type_var(),mk_list(2, mk_symbol("fn-exp"), $6)); }
  | VAL REC ID ':' type '=' FN match                                       { $$ = mk_list(4, mk_symbol("rec-dec"), mk_symbol($3), $5 , mk_list(2, mk_symbol("fn-exp"), $8)); }
  ;

 /*
TVARS ::= TVAR | "("TVAR{,TVAR}")"
 */

 /*
TBINDS ::= [TVARS] ID = TYPE {and [TVARS] ID = TYPE}
 */

 /*
DTBIND ::= [TVARS] ID = ID [of TYPE] {"|" ID [of TYPE]}
 */

 /*
FBIND ::= ID APAT {APAT} = EXP {"|" ID APAT {APAT} = EXP}
 */

 /*
Optional TVARS create local scope so type annotations avoid clashing with same-named type variables
outside. In FBIND, each occurrence of ID must be the same name each time. Unlike MATCH, FBIND
allows atomic patterns separated by spaces. From TYPE to PAT, rule order indicates precedence:
 */

 /*
TYPE ::= TVAR
| RTYPE
| BTYPE
| LID (L)               ID must name a type
| TYPE list
| TYPE ref
| TYPE array
| TYPE option
| TYPE vector
| TYPE LID
| "("TYPE{,TYPE}")"     LID ID is a datatype/abstype
| TYPE * TYPE           Builds tuple types
| TYPE -> TYPE          Builds function types
| "("TYPE")"            Note: (T*T)*T != T*T*T !
 */
type
  : btype                                                                  { $$ = $1;}
  | type THIN_ARROW type                                                   { $$ = mk_list(3, mk_symbol("proc-type"), $1, $3); }
  | '(' type ')'                                                           { $$ = $2; }
  ;

 /*
RTYPE ::= "{"LABEL: TYPE{, LABEL: TYPE}"}" Field names-part of type
 */

 /*
BTYPE ::= int | real | char | string | unit | exn | word | substring
| bool | order | TextIO.instream | TextIO.outstream (and BinIO."")
 */
btype
  : INT                                                                    { $$ = mk_list(1, mk_symbol("int-type"));}
  | BOOL                                                                   { $$ = mk_list(1, mk_symbol("bool-type"));}
  ;

 /*
expressions. Any ID, EXP, or PAT below may be followed by a colon : and a legal type; this binds
looser than before but tighter than andalso. Some lines are redundant—for suggestion and clarity.
 */

 /*
EXP ::=
CONST
| LID
| PREFIXFN
| PREFCON #LABEL                 passable as a fn
| #LABEL                         #2 (a,b) = b, #c {c=3, e=4} = 3
| "{"LABEL=EXP {, LABEL=EXP}"}"  record. Note {2=a, 1=b} = (b,a)
| "("EXP{, EXP}")"               tuple. Note "(EXP)" is legal
| "("EXP{; EXP}")"               sequence-of-"statements"
| "["EXP{, EXP}"]"               list of known length
| "#["EXP{,EXP}"]"               SML-NJ vector: unbreakable list
| let {DEC[;]} in EXP{;EXP} end  Makes a scope. Last EXP has no ;
| EXP EXP                        Includes PREFIXFN EXP. Just space, no comma!
| ! EXP                          Dereference--EXP must eval to a ref variable
| ref EXP                        Here EXP must have a non-polymorphic type
| EXP INFIXFN EXP                See top-level infix ops below, precedence 4-7
| EXP o EXP                      Function composition: literal o. Precedence 3
| EXP := EXP                     First EXP must eval to ref var. Precedence 3
| EXP before EXP                 Value is lhs; do rhs for side-effects. Prec.0
| EXP andalso EXP                Short-circuit Booleans are "special", not fns
| EXP orelse EXP
| EXP handle MATCH Catch exn.    EXP may need (...)
| raise EXP                      Throw an exception.
| if EXP then EXP else EXP       Just like "EXP ? EXP : EXP" in C
| while EXP do EXP               EXPs must use !, :=, or similar
| case EXP of MATCH              Often needs (...) around it
| fn MATCH                       Best use: anonymous functions
 */

exp
  : CONST_INT                                                              { $$ = mk_list(2, mk_symbol("const-int-exp"), mk_number_from_int(atoi($1))); }
  | ID                                                                     { $$ = mk_list(2, mk_symbol("id-exp"), mk_symbol($1)); }
  | '(' exp ')'                                                            { $$ = $2; }
  | exp exp                                                                { $$ = mk_list(3, mk_symbol("app-exp"), $1, $2);}   
  | exp '+' exp                                                            { $$ = mk_list(3, mk_symbol("add-exp"), $1, $3); }
  | exp '-' exp                                                            { $$ = mk_list(3, mk_symbol("diff-exp"), $1, $3); }
  | exp '*' exp                                                            { $$ = mk_list(3, mk_symbol("mul-exp"), $1, $3);}
  | exp '/' exp                                                            { $$ = mk_list(3, mk_symbol("div-exp"), $1, $3); }
  | exp '=' exp                                                            { $$ = mk_list(3, mk_symbol("equal-exp"), $1, $3);}
  | LET dec IN exp END                                                     { $$ = mk_list(3, mk_symbol("let-exp"), $2, $4);}
  | IF exp THEN exp ELSE exp                                               { $$ = mk_list(4, mk_symbol("if-exp"), $2, $4, $6); }
  | FN match                                                               { $$ = mk_list(2, mk_symbol("fn-exp"), $2); }
  ;


 /*
Matches and Patterns. Wildcard _ means “ignore”; no (other) ID may appear twice in a PAT.
 */

 /*
MATCH::= PAT => EXP {"|" PAT => EXP}
 */

match
:  pat FAT_ARROW exp                                                       { $$ = mk_list(3, mk_symbol("pat-match"), $1, $3); }
  ;

 /*
PAT ::= APAT atomic pattern
| PREFCON PAT                    pattern with a constructor
| PAT INFIXCON PAT               includes list pattern x::rest
| ID as                          PAT binds ID to a match for PAT
 */
pat
  : apat ':' type                                                          { $$ = mk_list(3, mk_symbol("apat-pat"), $1, $3); } 
  | apat                                                                   { $$ = mk_list(3, mk_symbol("apat-pat"), $1, mk_type_var()); } 
  ; 

 /*
APAT ::= CONST                   includes MONOCONs and MONOEXNs
| ID
| _                              ID is *written to*, not read!
| "(" PAT{, PAT} ")"             tuple pattern
| "(" APAT{"|" APAT} ")"         "OR-pattern", in SML-NJ only?
| "[]" | "[" PAT{,PAT} "]"       list pattern of fixed size
| "#[]" | "#[" APAT{,APAT} "]"   vector pattern, SML-NJ addition
| "{" FPAT{, FPAT}[, ...] "}"    record pattern, "..." wildcard
 */

apat
: ID                                                                       { $$ = mk_list(2, mk_symbol("id-apat"), mk_symbol($1)); display_debug($$); }
  ;

/*
FPAT ::= LABEL=PAT | ID [as PAT] ID names field & gets its value
*/

%%
