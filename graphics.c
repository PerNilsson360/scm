#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "common.h"
#include "type.h"
#include "error.h"
#include "util.h"
#include "eval.h"
#include "str.h"
#include "symbol.h"
#include "number.h"
#include "procedure.h"
#include "graphics.h"
#include "char.h"

static Display *display;
static Window win;
static GC gc;
static XFontStruct* font_struct;
static int screen_number;
static pthread_t gr_thread;
static unsigned int point_x = 0;
static unsigned int point_y = 0;

static int 
_get_height(const TYPE* exp, int display_height)
{
    int result;

    TYPE* height = assq(mk_symbol("height"), exp);
    
    if (is_nil(height) || is_nil(cdr(height)) || !is_number(car(cdr(height))))
    {
        result = display_height / 4;
    }
    else
    {
        result = car(cdr(height))->d.i;
    }
    
    return result;
}

static int 
_get_width(const TYPE* exp, int display_width)
{
    int result;

    TYPE* width = assq(mk_symbol("width"), exp);
    
    if (is_nil(width) || is_nil(cdr(width)) || !is_number(car(cdr(width))))
    {
        result = display_width / 4;
    }
    else
    {
        result = car(cdr(width))->d.i;
    }
    
    return result;
}

char*
_get_window_name(const TYPE* exp)
{
    char* result = "Scheme graphics";
    TYPE* name = assq(mk_symbol("window-name"), exp);
    
    if (!is_nil(name) && !is_nil(cdr(name)) && is_string(car(cdr(name))))
    {
        result = car(cdr(name))->d.s;
    }
    
    return result;
}

void
gr_open(const TYPE* exp)
{
    int x = 0; 
    int y = 0;
    int width, height, display_width, display_height;
    XSizeHints *size_hints;      /* need to free this */
    XWMHints *wm_hints;         /* need to free this */
    XEvent event;
    XTextProperty window_name;
    char* window_name_str;
    XGCValues gc_values;
    unsigned int events_pending;

    if ((display = XOpenDisplay(NULL)) == NULL )
    {
        fprintf(stderr, "GR-OPEN: can not open display");
        return;
    }

    screen_number = DefaultScreen(display);
    display_width = DisplayWidth(display, screen_number);
    display_height = DisplayHeight(display, screen_number);

    height = _get_height(exp, display_width);
    width = _get_width(exp, display_height);

    win = XCreateSimpleWindow(display, 
                              RootWindow(display, screen_number),
                              x, 
                              y, 
                              width, 
                              height, 
                              0, 
                              BlackPixel(display, screen_number), 
                              WhitePixel(display, screen_number));

    if ((size_hints = XAllocSizeHints()) == NULL) 
    {
        fprintf(stderr, "GR-OPEN: can not get mem for sizehints");
        exit(1);
    }
    
    if ((wm_hints = XAllocWMHints()) == NULL) 
    {
        fprintf(stderr, "GR-OPEN: can not get mem for sizehints");
        exit(1);
    }

    size_hints->flags = PPosition | PSize | PMinSize;
    size_hints->min_width = 300;
    size_hints->min_height = 200;

    wm_hints->initial_state = NormalState;
    wm_hints->input = True;
    wm_hints->flags = StateHint | InputHint;

    window_name_str = _get_window_name(exp);

    if (XStringListToTextProperty(&window_name_str, 1, &window_name) == 0) 
    {
        fprintf(stderr, 
                "GR-OPEN: structure allocation for windowName failed.\n");
        exit(1);
    }

    XSetWMProperties(display, 
                     win, 
                     &window_name, 
                     NULL,
                     NULL, 
                     0, 
                     size_hints, 
                     wm_hints,
                     0);

    /* Xfree(size_hints); */
    /* Xfree(wm_hints); */

    XSelectInput(display, 
                 win, 
                 ExposureMask | 
                 KeyPressMask |
                 ButtonPressMask | 
                 StructureNotifyMask);

    if ((font_struct = XLoadQueryFont(display, "fixed")) == NULL)
    {
        fprintf(stderr, "GR_OPEN: could not load font\n");
        exit(1);
    }

    gc_values.font = font_struct->fid;
    unsigned long bl = BlackPixel(display, screen_number);
    unsigned long wh = WhitePixel(display, screen_number);
    gc_values.foreground = bl ^ wh;
    gc_values.background = WhitePixel(display, screen_number);
    gc_values.function = GXxor;
    gc = XCreateGC(display, 
                   win, 
                   (GCFont | GCForeground | GCBackground | GCFunction),
                   &gc_values);

    XMapWindow(display, win);
    XFlush(display);
}

void 
gr_close()
{
    XCloseDisplay(display);
}

void 
gr_move_to(const TYPE* x, const TYPE* y)
{
    point_x = (unsigned int) x->d.i;
    point_y = (unsigned int) y->d.i;
}

void
gr_draw_char(const TYPE* exp)
{
    assert_throw(is_char(exp), 
                 TYPE_ERROR,
                 "GR_DRAW_CHAR: expects a char as argument");

    char string[2];
    string[0] = exp->d.i;
    string[1] = '\0';

    XDrawString(display,
                win,
                gc,
                point_x,
                point_y,
                string,
                strlen(string));
}

void 
gr_draw_string(const TYPE* exp)
{
    assert_throw(is_string(exp), 
                 TYPE_ERROR,
                 "GR_DRAW_STRING: expects a string as argument");

    XDrawString(display, 
                win, 
                gc,
                point_x,
                point_y,
                exp->d.s, 
                strlen(exp->d.s));
}

void 
gr_set_font(const TYPE* exp)
{
}

void 
gr_set_text_size(const TYPE* exp)
{
}

TYPE*
gr_text_size(const TYPE* exp)
{
    assert_throw(is_string(exp), 
                 TYPE_ERROR,
                 "GR_TEXT_SIZE: expects a string as argument");

    int text_width;
    int direction, ascent, descent;
    XCharStruct char_sizes;
    
    text_width = XTextWidth(font_struct, 
                            exp->d.s, 
                            strlen(exp->d.s));

    XTextExtents(font_struct,
                 exp->d.s,
                 strlen(exp->d.s),
                 &direction,
                 &ascent,
                 &descent,
                 &char_sizes);

    return cons(mk_number_from_int(text_width), 
                mk_number_from_int(ascent + descent));
}

void 
gr_set_foreground(const TYPE* exp)
{
    assert_throw(is_number(exp), 
                 TYPE_ERROR,
                 "GR_SET_FOREGROUND: expects a number as argument");

    XSetForeground(display, 
                   gc, 
                   (unsigned long) exp->d.i ^ WhitePixel(display, screen_number));
}

TYPE* 
x_events_queued()
{
    return mk_number_from_int(XEventsQueued(display, QueuedAfterReading));
}

TYPE*
x_next_event()
{
    TYPE* result;
    TYPE* window;
    XEvent event;

    XNextEvent(display, &event);
    window = mk_number_from_int(event.xany.window);

    switch (event.type) 
    {
    case Expose:
        result = cons(mk_symbol("expose"), 
                      cons(window, 
                           cons(mk_number_from_int(event.xexpose.x),
                                cons(mk_number_from_int(event.xexpose.y),
                                     cons(mk_number_from_int(event.xexpose.width),
                                          cons(mk_number_from_int(event.xexpose.height),
                                               nil()))))));
        break;
    case ButtonPress:
        result = cons(mk_symbol("button-press"),
                      cons(window, 
                           cons(mk_number_from_int(event.xbutton.x),
                           cons(mk_number_from_int(event.xbutton.y),
                                cons(mk_number_from_int(event.xbutton.state),
                                     cons(mk_number_from_int(event.xbutton.button),
                                          nil()))))));
        break;
    case KeyPress:
    {
        char buffer[10];
        int bufsize = 10;
        KeySym ks;
        int nchars = XLookupString(&event.xkey, buffer, bufsize, &ks, NULL);

        result = cons(mk_symbol("key-press"),
                      cons(window,
                           cons(mk_number_from_int(event.xkey.x),
                                cons(mk_number_from_int(event.xkey.y),
                                     cons(mk_number_from_int(event.xkey.state),
                                          cons(mk_number_from_int(ks),
                                               cons(mk_string_with_length(buffer, nchars),
                                                    nil())))))));
        break;
    }   
    default:
        result = cons(mk_symbol("not-implemented"), nil());
        break;
    }
    
    return result;
}

void 
x_fill_arc(const TYPE* drawable, 
           const TYPE* width, 
           const TYPE* height, 
           const TYPE* angle1, 
           const TYPE* angle2)
{
    assert_throw(is_number(drawable), 
                 TYPE_ERROR,
                 "GR_TEXT_SIZE: drawable is a number");

    assert_throw(is_number(width), 
                 TYPE_ERROR,
                 "GR_TEXT_SIZE: width is a number");

    assert_throw(is_number(height), 
                 TYPE_ERROR,
                 "GR_TEXT_SIZE: height is a number");

    assert_throw(is_number(angle1), 
                 TYPE_ERROR,
                 "GR_TEXT_SIZE: angle1 is a number");

    assert_throw(is_number(angle2), 
                 TYPE_ERROR,
                 "GR_TEXT_SIZE: angle2 is a number");


    XFillArc(display, 
             drawable->d.i,
             gc,
             point_x, 
             point_y, 
             width->d.i, 
             height->d.i, 
             angle1->d.i, 
             angle2->d.i);
}

void
x_flush()
{
    XFlush(display);
}

TYPE* 
gr_root_win()
{
    return mk_number_from_int(win);
}
