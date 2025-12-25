// MIT license
//
// Copyright 2025 Per Nilsson
///
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
#ifndef _PORT_H_
#define _PORT_H_

#include "type.h"

TYPE* is_port(const TYPE* obj);
TYPE* is_input_port(const TYPE* obj);
TYPE* is_output_port(const TYPE* obj);
TYPE* open_input_file(const TYPE* filename);
TYPE* open_output_file(const TYPE* filename);
void close_input_port(const TYPE* port);
void close_output_port(const TYPE* port);

/* SRFI 6 */

/* Specification */
/* This specification is taken from the MacScheme Reference Manual.  */
/* (OPEN-INPUT-STRING string)                            ;procedure */

/* Takes a string and returns an input port that delivers characters 
   from the string. The port can be closed by CLOSE-INPUT-PORT, though 
   its storage will be reclaimed by the garbage collector if it 
   becomes inaccessible.  */
/*         (define p */
/*           (open-input-string "(a . (b . (c . ()))) 34")) */

/*         (input-port? p)                 -->  #t */
/*         (read p)                        -->  (a b c) */
/*         (read p)                        -->  34 */
/*         (eof-object? (peek-char p))     -->  #t */

/* (OPEN-OUTPUT-STRING)                                  ;procedure */

/* Returns an output port that will accumulate characters for retrieval by 
   GET-OUTPUT-STRING. The port can be closed by the procedure 
   CLOSE-OUTPUT-PORT, though its storage will be reclaimed by the 
   garbage collector if it becomes inaccessible.  */
/*         (let ((q (open-output-string)) */
/*               (x '(a b c))) */
/*           (write (car x) q) */
/*           (write (cdr x) q) */
/*           (get-output-string q))        -->  "a(b c)" */

/* (GET-OUTPUT-STRING output-port)                       ;procedure */

/* Given an output port created by OPEN-OUTPUT-STRING, returns a string consisting of the characters that have been output to the port so far.  */

#endif
