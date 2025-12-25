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
#ifndef _GRAPHICS_H_
#define _GRAPHICS_H_

#include "type.h"

void gr_init();

/*
 * @param exp is association list that understands height, width 
 *            and window-name                        
 */
void gr_open(const TYPE* exp);
void gr_close();
void gr_move_to(const TYPE* x, const TYPE* y); 
void gr_draw_char(const TYPE* exp);
void gr_draw_string(const TYPE* exp);
void gr_set_font(const TYPE* exp);      /* string */
void gr_set_text_size(const TYPE* exp); /* int */
TYPE* gr_text_size();                   /* (pair int int) */
void gr_draw_point();
void gr_draw_line(const TYPE* x1, const TYPE* y1, const TYPE* x2, const TYPE* y2);
void gr_set_foreground(const TYPE* exp); /* symbol */
TYPE* x_events_queued();
TYPE* x_next_event();
void x_flush();
void gr_fill_rect(const TYPE* width, const TYPE* height);
void gr_fill_arc(const TYPE* width, 
		 const TYPE* height, 
		 const TYPE* angle1, 
		 const TYPE* angle2);
void gr_fill_polygon(const TYPE* points); /* #((1 . 2)(3 . 4)) i.e. array of pairs */
void gr_swap_buffers();

#endif
