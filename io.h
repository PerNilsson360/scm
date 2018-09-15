#ifndef _IO_H_
#define _IO_H_

#include "type.h" 

TYPE* read();
TYPE* read_from_port(const TYPE* port);
TYPE* read_char_from_port(const TYPE* port);
TYPE* peek_char_from_port(const TYPE* port);
int is_eof_object(const TYPE* sexp);
void display(const TYPE* sexp);
void newline();
void error(const TYPE* sexp);
void display_debug(const TYPE* sexp);

#endif
