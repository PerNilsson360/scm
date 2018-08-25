#ifndef _GRAPHICS_H_
#define _GRAPHICS_H_

#include "type.h"

/*
 * @param exp is association list that understands height, width 
 *            and window-name                        
 */
void gr_open(const type* exp);
void gr_close();
void gr_move_to(const type* x, const type* y); 
void gr_draw_char(const type* exp);
void gr_draw_string(const type* exp);
void gr_set_font(const type* exp);      /* string */
void gr_set_text_size(const type* exp); /* int */
type* gr_text_size();                   /* (pair int int) */ 
void gr_set_foreground(const type* exp); /* int */
type* x_events_queued();
type* x_next_event();
void x_flush();
void x_fill_arc(const type* drawable, 
                const type* width, 
                const type* height, 
                const type* angle1, 
                const type* angle2);
type* gr_root_win();

#endif
