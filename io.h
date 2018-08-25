#ifndef _IO_H_
#define _IO_H_

#include "type.h" 

type* read();
type* read_from_port(const type* port);
type* read_char_from_port(const type* port);
type* peek_char_from_port(const type* port);
int is_eof_object(const type* sexp);
void display(const type* sexp);
void newline();
void error(const type* sexp);
void display_debug(const type* sexp);

#endif
