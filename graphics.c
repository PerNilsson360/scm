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
#include "vector.h"

static Display* display = NULL;
static Window win;
static GC gc;
static XFontStruct* font_struct;
static int screen_number;
static int width;
static int height;
static pthread_t gr_thread;
static unsigned int point_x = 0;
static unsigned int point_y = 0;
static Pixmap pixmap;

static int 
_get_height(const TYPE* exp, int display_height)
{
    int result;

    TYPE* height = assq(mk_symbol("height"), exp);
    
    if (IS_NIL(height) || IS_NIL(cdr(height)) || !is_number(car(cdr(height))))
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
    
    if (IS_NIL(width) || IS_NIL(cdr(width)) || !is_number(car(cdr(width))))
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
    
    if (!IS_NIL(name) && !IS_NIL(cdr(name)) && is_string(car(cdr(name))))
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
    int display_width, display_height;
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

	XWindowAttributes attrib;
	int status = XGetWindowAttributes(display, win, & attrib);
	fprintf(stderr, "status %d depth %d\n", status, attrib.depth);
	
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
    gc_values.foreground = bl;
    gc_values.background = WhitePixel(display, screen_number);
    gc_values.function = GXcopy;
    gc = XCreateGC(display, 
                   win, 
                   (GCFont | GCForeground | GCBackground | GCFunction),
                   &gc_values);

    XMapWindow(display, win);
	pixmap = XCreatePixmap(display, win, width, height, 24);
    XFlush(display);
}

void 
gr_close()
{
    XCloseDisplay(display);
	display = 0;
}

void 
gr_move_to(const TYPE* x, const TYPE* y)
{
    point_x = (unsigned int) as_integer(x);
    point_y = (unsigned int) as_integer(y);
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
                pixmap,
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
                pixmap, 
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
    assert_throw(is_integer(exp), 
                 TYPE_ERROR,
                 "GR_SET_FOREGROUND: expects a integer as argument");

    XSetForeground(display, 
                   gc, 
                   (unsigned long) exp->d.i);
}

void
gr_draw_point()
{
	fprintf(stderr, "%d %d\n", point_x, point_y);
	int rc = XDrawPoint(display, pixmap, gc, point_x, point_y);

	if (rc == 0)
	{
		char buff[256];
		XGetErrorText(display, rc, buff, 256);
		fprintf(stderr, "GR_DRAW_point: error %s.\n", buff);
	}
}

void
gr_draw_line(const TYPE* x1, const TYPE* y1, const TYPE* x2, const TYPE* y2)
{
	assert_throw(is_number(x1), 
                 TYPE_ERROR,
                 "GR_SET_FOREGROUND: x1 is not an integer");
	assert_throw(is_number(y1), 
				 TYPE_ERROR,
                 "GR_SET_FOREGROUND: y1 is not an integer");
	assert_throw(is_number (x2), 
                 TYPE_ERROR,
                 "GR_SET_FOREGROUND: x2 is not an integer");
	assert_throw(is_number(y2), 
                 TYPE_ERROR,
                 "GR_SET_FOREGROUND: y1 is not an integer");

	if (display == 0) {
		fprintf(stderr, "GR_DRAW_LINE: need to call gr-open.\n");
		return;
	}
	
	int rc = XDrawLine(display,
					   pixmap,
					   gc,
					   as_integer(x1),
					   as_integer(y1),
					   as_integer(x2),
					   as_integer(y2));
	
	if (rc == 0)
	{
		char buff[256];
		XGetErrorText(display, rc, buff, 256);
		fprintf(stderr, "GR_DRAW_LINE: error %s.\n", buff);
	}
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
        result = cons(mk_symbol("not-implemented-event"), nil());
        break;
    }
    
    return result;
}

void
gr_fill_rect(const TYPE* width, const TYPE* height) {
	assert_throw(is_integer(width), 
				 TYPE_ERROR,
                 "GR_FILL_RECTANGLE: width must be an integer");
	
	assert_throw(is_integer(height), 
				 TYPE_ERROR,
                 "GR_FILL_RECTANGLE: height must be an integer");
	
	int status = XFillRectangle(display,
								pixmap,
								gc,
								point_x,
								point_y,
								width->d.i,
								height->d.i);
	if (status == 0)
	{
		char buff[256];
		XGetErrorText(display, status, buff, 256);
		fprintf(stderr, "GR_FILL_RECT: error %s.\n", buff);
	}
}


void 
gr_fill_arc(const TYPE* width, 
			const TYPE* height, 
			const TYPE* angle1, 
			const TYPE* angle2)
{
    assert_throw(is_integer(width), 
                 TYPE_ERROR,
                 "GR_FILL_ARC: width is an integer");

    assert_throw(is_integer(height), 
                 TYPE_ERROR,
                 "GR_FILL_ARC: height is a integer");

    assert_throw(is_integer(angle1), 
                 TYPE_ERROR,
                 "GR_FILL_ARC: angle1 is a integer");

    assert_throw(is_integer(angle2), 
                 TYPE_ERROR,
                 "GR_FILL_ARC: angle2 is a integer");

    int status = XFillArc(display, 
						  pixmap,
						  gc,
						  point_x, 
						  point_y, 
						  width->d.i, 
						  height->d.i, 
						  angle1->d.i, 
						  angle2->d.i);
	if (status == 0)
	{
		char buff[256];
		XGetErrorText(display, status, buff, 256);
		fprintf(stderr, "GR_FILL_ARC: error %s.\n", buff);
	}
}

void gr_fill_polygon(const TYPE* points)
{
    assert_throw(is_vector(points),
                 TYPE_ERROR,
                 "GR_FILL_POLYGON: points must be a vector");
    
    int n_points = points->d.v->length->d.i;
    XPoint* ps = mloc(sizeof(XPoint) * n_points);
    for (int i = 0; i < n_points; i++) {
        TYPE* point = vector_ref(points, i);
        assert_throw(is_pair(point),
                     TYPE_ERROR,
                     "GR_FILL_POLYGON: point must be a pair");
        TYPE* x = car(point);
        assert_throw(is_integer(x),
                     TYPE_ERROR,
                     "GR_FILL_POLYGON: x must be an integer");
        TYPE* y = cdr(point);
        assert_throw(is_integer(x),
                     TYPE_ERROR,
                     "GR_FILL_POLYGON: y must be an integer");
        ps[i].x = x->d.i;
        ps[i].y = y->d.i;
        fprintf(stderr, "point %d %d ", ps[i].x, ps[i].y);
    }

    fprintf(stderr, "n points %d\n", n_points);
    
    int status = XFillPolygon(display,
                              pixmap,
                              gc,
                              ps,
                              n_points,
                              Convex,
                              CoordModeOrigin);
	if (status == 0)
	{
		char buff[256];
		XGetErrorText(display, status, buff, 256);
		fprintf(stderr, "GR_FILL_POLYGON: error %s.\n", buff);
	}
}

void
x_flush()
{
    XFlush(display);
}

void
gr_swap_buffers()
{
    int status = XCopyArea(display, pixmap, win, gc, 0, 0, width, height, 0,0);

	if (status == 0)
	{
		char buff[256];
		XGetErrorText(display, status, buff, 256);
		fprintf(stderr, "GR_SWAP_BUFFERS: error %s.\n", buff);
	}
    
    XSync(display, False);
}
