#ifndef _GRAPHICS_H_
#define _GRAPHICS_H_

#include "type.h"

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
void gr_set_foreground(const TYPE* exp); /* int */
TYPE* x_events_queued();
TYPE* x_next_event();
void x_flush();
void x_fill_arc(const TYPE* drawable, 
                const TYPE* width, 
                const TYPE* height, 
                const TYPE* angle1, 
                const TYPE* angle2);
TYPE* gr_root_win();

#endif
