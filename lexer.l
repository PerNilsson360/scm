D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%{
#include <stdio.h>
#include "error.h"	
#include "type.h"
#include "symbol.h"
#include "port.h"

#include "y.tab.h"

void count();
void comment();
extern TYPE* syntax_tree;
%}

%%
"(*"			{ comment(); }

"abstype"		{ count(); return(ABSTYPE); }
"and"			{ count(); return(AND); }
"andalso"		{ count(); return(ANDALSO); }
"as"     		{ count(); return(AS); }
"case"			{ count(); return(CASE); }
"do"			{ count(); return(DO); }
"datatype"		{ count(); return(DATATYPE); }
"else"			{ count(); return(ELSE); }
"end"			{ count(); return(END); }
"exception"		{ count(); return(EXCEPTION); }
"fn"			{ count(); return(FN); }
"fun"    		{ count(); return(FUN); }
"handle"		{ count(); return(HANDLE); }
"if"    		{ count(); return(IF); }
"in"    		{ count(); return(IN); }
"infix"			{ count(); return(INFIX); }
"infixr"		{ count(); return(INFIXR); }
"let "          { count(); return(LET); }
"of"			{ count(); return(OF); }
"op"			{ count(); return(OP); }
"open"  		{ count(); return(OPEN); }
"orelse"		{ count(); return(ORELSE); }
"rec"   		{ count(); return(REC); }
"then"  		{ count(); return(THEN); }
"type"  		{ count(); return(SML_TYPE); }
"val"   		{ count(); return(VAL); }
"with"  		{ count(); return(WITH); }
"withtype"  	{ count(); return(WITHTYPE); }
"while"  		{ count(); return(WHILE); }
"=>"		    { count(); return(FAT_ARROW); }
"->"            { count(); return(THIN_ARROW); }
"INT"           { count(); return(INT); }
"BOOL"          { count(); return(BOOL); }

"("			    { count(); return('('); }
")"			    { count(); return(')'); }
"["			    { count(); return('['); }
"]"			    { count(); return(']'); }
","			    { count(); return(','); }
":"			    { count(); return(':'); }
";"			    { count(); return(';'); }
"..."		    { count(); return(ELIPSIS); }
"_"			    { count(); return('_'); }
"|"			    { count(); return('|'); }
"="			    { count(); return('='); }
"-"             { count(); return('-'); }
"+"             { count(); return('+'); }
"*"             { count(); return('*'); }
"/"             { count(); return('/'); }
"#"             { count(); return('#'); }

{L}({L}|{D})*		{ count(); yylval.sval = strdup(yytext); return(ID); }
{D}+  		    { count(); yylval.sval = strdup(yytext); return(CONST_INT); }
[ \t\v\n\f]		{ count(); }
.			{ /* ignore bad characters */ }

%%

int
yywrap()
{
    fprintf(stderr, "yywrap man \n");
	return(1);
}

void
comment()
{
	char c, c1;

loop:
	while ((c = input()) != '*' && c != 0)
		putchar(c);

	if ((c1 = input()) != ')' && c != 0)
	{
		unput(c1);
		goto loop;
	}

	if (c != 0)
		putchar(c1);
}


int column = 0;

void count()
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
		if (yytext[i] == '\n')
			column = 0;
		else if (yytext[i] == '\t')
			column += 8 - (column % 8);
		else
			column++;

	ECHO;
}


int check_type()
{
/*
* pseudo code --- this is what it should check
*
*	if (yytext == type_name)
*		return(TYPE_NAME);
*
*	return(IDENTIFIER);
*/

/*
*	it actually will only return IDENTIFIER
*/

	return(ID);
}


TYPE*
read_sml()
{
    unsigned int BUFF_SIZE = 1024;
    char buff[BUFF_SIZE];
    unsigned int i = 0;
    while (1) {
       int c = getc(stdin);
       buff[i++] = c;
       if (c == ';') {
          break;
       }
    }
    buff[i] = '\0';
    fprintf(stderr, "%i %s\n", i, buff);
    yy_switch_to_buffer(yy_scan_string(buff));
    yyparse();
	return syntax_tree;
}

TYPE*
read_sml_from_port(const TYPE* port)
{
	assert_throw(is_true(is_input_port(port)),
				 TYPE_ERROR,
                 "READ_SML_FROM_PORT: port is not an input port");
   printf("##############\n");
	yyin = port->d.po->file;
    yy_switch_to_buffer(yy_create_buffer( yyin, YY_BUF_SIZE ));
	yyparse();
	return syntax_tree;
}
