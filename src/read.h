#ifndef _READ_H_
#define _READ_H_

#include "type.h"

TYPE* scm_read();
TYPE* read_from_file(FILE* file);
TYPE* read_from_port(const TYPE* port);
TYPE* read_char();
TYPE* read_char_from_port(const TYPE* port);
TYPE* peek_char();
TYPE* peek_char_from_port(const TYPE* port);
void write_char_to_port(const TYPE* port, const TYPE* c);
TYPE* read_sml();
TYPE* read_sml_from_port(const TYPE* port);

#endif
