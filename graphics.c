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
#include "hash_table.h"

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
static TYPE* _colour_table_ = NULL;
static TYPE* _key_table_ = NULL;


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
gr_init() {
    _colour_table_ = mk_hash_table(_symbol_equal_, _symbol_hash_);
    hash_table_set(_colour_table_, mk_symbol("indian-red"), mk_number_from_int(0xb0171f)); 
    hash_table_set(_colour_table_, mk_symbol("crimson"), mk_number_from_int(0xdc143c)); 
    hash_table_set(_colour_table_, mk_symbol("lightpink"), mk_number_from_int(0xffb6c1)); 
    hash_table_set(_colour_table_, mk_symbol("lightpink-1"), mk_number_from_int(0xffaeb9)); 
    hash_table_set(_colour_table_, mk_symbol("lightpink-2"), mk_number_from_int(0xeea2ad)); 
    hash_table_set(_colour_table_, mk_symbol("lightpink-3"), mk_number_from_int(0xcd8c95)); 
    hash_table_set(_colour_table_, mk_symbol("lightpink-4"), mk_number_from_int(0x8b5f65)); 
    hash_table_set(_colour_table_, mk_symbol("pink"), mk_number_from_int(0xffc0cb)); 
    hash_table_set(_colour_table_, mk_symbol("pink-1"), mk_number_from_int(0xffb5c5)); 
    hash_table_set(_colour_table_, mk_symbol("pink-2"), mk_number_from_int(0xeea9b8)); 
    hash_table_set(_colour_table_, mk_symbol("pink-3"), mk_number_from_int(0xcd919e)); 
    hash_table_set(_colour_table_, mk_symbol("pink-4"), mk_number_from_int(0x8b636c)); 
    hash_table_set(_colour_table_, mk_symbol("palevioletred"), mk_number_from_int(0xdb7093)); 
    hash_table_set(_colour_table_, mk_symbol("palevioletred-1"), mk_number_from_int(0xff82ab));
    hash_table_set(_colour_table_, mk_symbol("palevioletred-2"), mk_number_from_int(0xee799f)); 
    hash_table_set(_colour_table_, mk_symbol("palevioletred-3"), mk_number_from_int(0xcd6889)); 
    hash_table_set(_colour_table_, mk_symbol("palevioletred-4"), mk_number_from_int(0x8b475d)); 
    hash_table_set(_colour_table_, mk_symbol("lavenderblush-1"), mk_number_from_int(0xfff0f5)); 
    hash_table_set(_colour_table_, mk_symbol("lavenderblush-2"), mk_number_from_int(0xeee0e5)); 
    hash_table_set(_colour_table_, mk_symbol("lavenderblush-3"), mk_number_from_int(0xcdc1c5)); 
    hash_table_set(_colour_table_, mk_symbol("lavenderblush-4"), mk_number_from_int(0x8b8386)); 
    hash_table_set(_colour_table_, mk_symbol("violetred-1"), mk_number_from_int(0xff3e96)); 
    hash_table_set(_colour_table_, mk_symbol("violetred-2"), mk_number_from_int(0xee3a8c)); 
    hash_table_set(_colour_table_, mk_symbol("violetred-3"), mk_number_from_int(0xcd3278)); 
    hash_table_set(_colour_table_, mk_symbol("violetred-4"), mk_number_from_int(0x8b2252)); 
    hash_table_set(_colour_table_, mk_symbol("hotpink"), mk_number_from_int(0xff69b4)); 
    hash_table_set(_colour_table_, mk_symbol("hotpink-1"), mk_number_from_int(0xff6eb4)); 
    hash_table_set(_colour_table_, mk_symbol("hotpink-2"), mk_number_from_int(0xee6aa7)); 
    hash_table_set(_colour_table_, mk_symbol("hotpink-3"), mk_number_from_int(0xcd6090)); 
    hash_table_set(_colour_table_, mk_symbol("hotpink-4"), mk_number_from_int(0x8b3a62)); 
    hash_table_set(_colour_table_, mk_symbol("raspberry"), mk_number_from_int(0x872657)); 
    hash_table_set(_colour_table_, mk_symbol("deeppink-1 "), mk_number_from_int(0xff1493)); 
    hash_table_set(_colour_table_, mk_symbol("deeppink-2"), mk_number_from_int(0xee1289)); 
    hash_table_set(_colour_table_, mk_symbol("deeppink-3"), mk_number_from_int(0xcd1076)); 
    hash_table_set(_colour_table_, mk_symbol("deeppink-4"), mk_number_from_int(0x8b0a50)); 
    hash_table_set(_colour_table_, mk_symbol("maroon-1"), mk_number_from_int(0xff34b3)); 
    hash_table_set(_colour_table_, mk_symbol("maroon-2"), mk_number_from_int(0xee30a7)); 
    hash_table_set(_colour_table_, mk_symbol("maroon-3"), mk_number_from_int(0xcd2990)); 
    hash_table_set(_colour_table_, mk_symbol("maroon-4"), mk_number_from_int(0x8b1c62)); 
    hash_table_set(_colour_table_, mk_symbol("mediumvioletred"), mk_number_from_int(0xc71585)); 
    hash_table_set(_colour_table_, mk_symbol("violetred"), mk_number_from_int(0xd02090)); 
    hash_table_set(_colour_table_, mk_symbol("orchid"), mk_number_from_int(0xda70d6)); 
    hash_table_set(_colour_table_, mk_symbol("orchid-1"), mk_number_from_int(0xff83fa)); 
    hash_table_set(_colour_table_, mk_symbol("orchid-2"), mk_number_from_int(0xee7ae9)); 
    hash_table_set(_colour_table_, mk_symbol("orchid-3"), mk_number_from_int(0xcd69c9)); 
    hash_table_set(_colour_table_, mk_symbol("orchid-4"), mk_number_from_int(0x8b4789)); 
    hash_table_set(_colour_table_, mk_symbol("thistle"), mk_number_from_int(0xd8bfd8)); 
    hash_table_set(_colour_table_, mk_symbol("thistle-1"), mk_number_from_int(0xffe1ff)); 
    hash_table_set(_colour_table_, mk_symbol("thistle-2"), mk_number_from_int(0xeed2ee)); 
    hash_table_set(_colour_table_, mk_symbol("thistle-3"), mk_number_from_int(0xcdb5cd)); 
    hash_table_set(_colour_table_, mk_symbol("thistle-4"), mk_number_from_int(0x8b7b8b)); 
    hash_table_set(_colour_table_, mk_symbol("plum-1"), mk_number_from_int(0xffbbff)); 
    hash_table_set(_colour_table_, mk_symbol("plum-2"), mk_number_from_int(0xeeaeee)); 
    hash_table_set(_colour_table_, mk_symbol("plum-3"), mk_number_from_int(0xcd96cd)); 
    hash_table_set(_colour_table_, mk_symbol("plum-4"), mk_number_from_int(0x8b668b)); 
    hash_table_set(_colour_table_, mk_symbol("plum"), mk_number_from_int(0xdda0dd)); 
    hash_table_set(_colour_table_, mk_symbol("violet"), mk_number_from_int(0xee82ee)); 
    hash_table_set(_colour_table_, mk_symbol("magenta"), mk_number_from_int(0xff00ff)); 
    hash_table_set(_colour_table_, mk_symbol("magenta-2"), mk_number_from_int(0xee00ee)); 
    hash_table_set(_colour_table_, mk_symbol("magenta-3"), mk_number_from_int(0xcd00cd)); 
    hash_table_set(_colour_table_, mk_symbol("magenta-4"), mk_number_from_int(0x8b008b)); 
    hash_table_set(_colour_table_, mk_symbol("purple "), mk_number_from_int(0x800080)); 
    hash_table_set(_colour_table_, mk_symbol("mediumorchid"), mk_number_from_int(0xba55d3)); 
    hash_table_set(_colour_table_, mk_symbol("mediumorchid-1"), mk_number_from_int(0xe066ff)); 
    hash_table_set(_colour_table_, mk_symbol("mediumorchid-2"), mk_number_from_int(0xd15fee)); 
    hash_table_set(_colour_table_, mk_symbol("mediumorchid-3"), mk_number_from_int(0xb452cd)); 
    hash_table_set(_colour_table_, mk_symbol("mediumorchid-4"), mk_number_from_int(0x7a378b)); 
    hash_table_set(_colour_table_, mk_symbol("darkviolet"), mk_number_from_int(0x9400d3)); 
    hash_table_set(_colour_table_, mk_symbol("darkorchid"), mk_number_from_int(0x9932cc)); 
    hash_table_set(_colour_table_, mk_symbol("darkorchid-1"), mk_number_from_int(0xbf3eff)); 
    hash_table_set(_colour_table_, mk_symbol("darkorchid-2"), mk_number_from_int(0xb23aee)); 
    hash_table_set(_colour_table_, mk_symbol("darkorchid-3"), mk_number_from_int(0x9a32cd)); 
    hash_table_set(_colour_table_, mk_symbol("darkorchid-4"), mk_number_from_int(0x68228b)); 
    hash_table_set(_colour_table_, mk_symbol("indigo"), mk_number_from_int(0x4b0082)); 
    hash_table_set(_colour_table_, mk_symbol("blueviolet"), mk_number_from_int(0x8a2be2)); 
    hash_table_set(_colour_table_, mk_symbol("purple-1"), mk_number_from_int(0x9b30ff)); 
    hash_table_set(_colour_table_, mk_symbol("purple-2"), mk_number_from_int(0x912cee)); 
    hash_table_set(_colour_table_, mk_symbol("purple-3"), mk_number_from_int(0x7d26cd)); 
    hash_table_set(_colour_table_, mk_symbol("purple-4"), mk_number_from_int(0x551a8b)); 
    hash_table_set(_colour_table_, mk_symbol("mediumpurple"), mk_number_from_int(0x9370db)); 
    hash_table_set(_colour_table_, mk_symbol("mediumpurple-1"), mk_number_from_int(0xab82ff)); 
    hash_table_set(_colour_table_, mk_symbol("mediumpurple-2"), mk_number_from_int(0x9f79ee)); 
    hash_table_set(_colour_table_, mk_symbol("mediumpurple-3"), mk_number_from_int(0x8968cd)); 
    hash_table_set(_colour_table_, mk_symbol("mediumpurple-4"), mk_number_from_int(0x5d478b)); 
    hash_table_set(_colour_table_, mk_symbol("darkslateblue"), mk_number_from_int(0x483d8b)); 
    hash_table_set(_colour_table_, mk_symbol("lightslateblue"), mk_number_from_int(0x8470ff)); 
    hash_table_set(_colour_table_, mk_symbol("mediumslateblue"), mk_number_from_int(0x7b68ee)); 
    hash_table_set(_colour_table_, mk_symbol("slateblue"), mk_number_from_int(0x6a5acd)); 
    hash_table_set(_colour_table_, mk_symbol("slateblue-1"), mk_number_from_int(0x836fff)); 
    hash_table_set(_colour_table_, mk_symbol("slateblue-2"), mk_number_from_int(0x7a67ee)); 
    hash_table_set(_colour_table_, mk_symbol("slateblue-3"), mk_number_from_int(0x6959cd)); 
    hash_table_set(_colour_table_, mk_symbol("slateblue-4"), mk_number_from_int(0x473c8b)); 
    hash_table_set(_colour_table_, mk_symbol("ghostwhite"), mk_number_from_int(0xf8f8ff)); 
    hash_table_set(_colour_table_, mk_symbol("lavender"), mk_number_from_int(0xe6e6fa)); 
    hash_table_set(_colour_table_, mk_symbol("blue"), mk_number_from_int(0x0000ff)); 
    hash_table_set(_colour_table_, mk_symbol("blue-2"), mk_number_from_int(0x0000ee)); 
    hash_table_set(_colour_table_, mk_symbol("blue-3"), mk_number_from_int(0x0000cd)); 
    hash_table_set(_colour_table_, mk_symbol("blue-4"), mk_number_from_int(0x00008b)); 
    hash_table_set(_colour_table_, mk_symbol("navy"), mk_number_from_int(0x000080)); 
    hash_table_set(_colour_table_, mk_symbol("midnightblue"), mk_number_from_int(0x191970)); 
    hash_table_set(_colour_table_, mk_symbol("cobalt"), mk_number_from_int(0x3d59ab)); 
    hash_table_set(_colour_table_, mk_symbol("royalblue"), mk_number_from_int(0x4169e1)); 
    hash_table_set(_colour_table_, mk_symbol("royalblue-1"), mk_number_from_int(0x4876ff)); 
    hash_table_set(_colour_table_, mk_symbol("royalblue-2"), mk_number_from_int(0x436eee)); 
    hash_table_set(_colour_table_, mk_symbol("royalblue-3"), mk_number_from_int(0x3a5fcd)); 
    hash_table_set(_colour_table_, mk_symbol("royalblue-4"), mk_number_from_int(0x27408b)); 
    hash_table_set(_colour_table_, mk_symbol("cornflowerblue"), mk_number_from_int(0x6495ed)); 
    hash_table_set(_colour_table_, mk_symbol("lightsteelblue"), mk_number_from_int(0xb0c4de)); 
    hash_table_set(_colour_table_, mk_symbol("lightsteelblue-1"), mk_number_from_int(0xcae1ff)); 
    hash_table_set(_colour_table_, mk_symbol("lightsteelblue-2"), mk_number_from_int(0xbcd2ee)); 
    hash_table_set(_colour_table_, mk_symbol("lightsteelblue-3"), mk_number_from_int(0xa2b5cd)); 
    hash_table_set(_colour_table_, mk_symbol("lightsteelblue-4"), mk_number_from_int(0x6e7b8b)); 
    hash_table_set(_colour_table_, mk_symbol("lightslategray"), mk_number_from_int(0x778899)); 
    hash_table_set(_colour_table_, mk_symbol("slategray"), mk_number_from_int(0x708090)); 
    hash_table_set(_colour_table_, mk_symbol("slategray-1"), mk_number_from_int(0xc6e2ff)); 
    hash_table_set(_colour_table_, mk_symbol("slategray-2"), mk_number_from_int(0xb9d3ee)); 
    hash_table_set(_colour_table_, mk_symbol("slategray-3"), mk_number_from_int(0x9fb6cd)); 
    hash_table_set(_colour_table_, mk_symbol("slategray-4"), mk_number_from_int(0x6c7b8b)); 
    hash_table_set(_colour_table_, mk_symbol("dodgerblue-1"), mk_number_from_int(0x1e90ff)); 
    hash_table_set(_colour_table_, mk_symbol("dodgerblue-2"), mk_number_from_int(0x1c86ee)); 
    hash_table_set(_colour_table_, mk_symbol("dodgerblue-3"), mk_number_from_int(0x1874cd)); 
    hash_table_set(_colour_table_, mk_symbol("dodgerblue-4"), mk_number_from_int(0x104e8b)); 
    hash_table_set(_colour_table_, mk_symbol("aliceblue"), mk_number_from_int(0xf0f8ff)); 
    hash_table_set(_colour_table_, mk_symbol("steelblue"), mk_number_from_int(0x4682b4)); 
    hash_table_set(_colour_table_, mk_symbol("steelblue-1"), mk_number_from_int(0x63b8ff)); 
    hash_table_set(_colour_table_, mk_symbol("steelblue-2"), mk_number_from_int(0x5cacee)); 
    hash_table_set(_colour_table_, mk_symbol("steelblue-3"), mk_number_from_int(0x4f94cd)); 
    hash_table_set(_colour_table_, mk_symbol("steelblue-4"), mk_number_from_int(0x36648b)); 
    hash_table_set(_colour_table_, mk_symbol("lightskyblue"), mk_number_from_int(0x87cefa)); 
    hash_table_set(_colour_table_, mk_symbol("lightskyblue-1"), mk_number_from_int(0xb0e2ff)); 
    hash_table_set(_colour_table_, mk_symbol("lightskyblue-2"), mk_number_from_int(0xa4d3ee)); 
    hash_table_set(_colour_table_, mk_symbol("lightskyblue-3"), mk_number_from_int(0x8db6cd)); 
    hash_table_set(_colour_table_, mk_symbol("lightskyblue-4"), mk_number_from_int(0x607b8b)); 
    hash_table_set(_colour_table_, mk_symbol("skyblue-1"), mk_number_from_int(0x87ceff)); 
    hash_table_set(_colour_table_, mk_symbol("skyblue-2"), mk_number_from_int(0x7ec0ee)); 
    hash_table_set(_colour_table_, mk_symbol("skyblue-3"), mk_number_from_int(0x6ca6cd)); 
    hash_table_set(_colour_table_, mk_symbol("skyblue-4"), mk_number_from_int(0x4a708b)); 
    hash_table_set(_colour_table_, mk_symbol("skyblue"), mk_number_from_int(0x87ceeb)); 
    hash_table_set(_colour_table_, mk_symbol("deepskyblue-1"), mk_number_from_int(0x00bfff)); 
    hash_table_set(_colour_table_, mk_symbol("deepskyblue-2"), mk_number_from_int(0x00b2ee)); 
    hash_table_set(_colour_table_, mk_symbol("deepskyblue-3"), mk_number_from_int(0x009acd)); 
    hash_table_set(_colour_table_, mk_symbol("deepskyblue-4"), mk_number_from_int(0x00688b)); 
    hash_table_set(_colour_table_, mk_symbol("peacock"), mk_number_from_int(0x33a1c9)); 
    hash_table_set(_colour_table_, mk_symbol("lightblue"), mk_number_from_int(0xadd8e6)); 
    hash_table_set(_colour_table_, mk_symbol("lightblue-1"), mk_number_from_int(0xbfefff)); 
    hash_table_set(_colour_table_, mk_symbol("lightblue-2"), mk_number_from_int(0xb2dfee)); 
    hash_table_set(_colour_table_, mk_symbol("lightblue-3"), mk_number_from_int(0x9ac0cd)); 
    hash_table_set(_colour_table_, mk_symbol("lightblue-4"), mk_number_from_int(0x68838b)); 
    hash_table_set(_colour_table_, mk_symbol("powderblue"), mk_number_from_int(0xb0e0e6)); 
    hash_table_set(_colour_table_, mk_symbol("cadetblue-1"), mk_number_from_int(0x98f5ff)); 
    hash_table_set(_colour_table_, mk_symbol("cadetblue-2"), mk_number_from_int(0x8ee5ee)); 
    hash_table_set(_colour_table_, mk_symbol("cadetblue-3"), mk_number_from_int(0x7ac5cd)); 
    hash_table_set(_colour_table_, mk_symbol("cadetblue-4"), mk_number_from_int(0x53868b)); 
    hash_table_set(_colour_table_, mk_symbol("turquoise-1"), mk_number_from_int(0x00f5ff)); 
    hash_table_set(_colour_table_, mk_symbol("turquoise-2"), mk_number_from_int(0x00e5ee)); 
    hash_table_set(_colour_table_, mk_symbol("turquoise-3"), mk_number_from_int(0x00c5cd)); 
    hash_table_set(_colour_table_, mk_symbol("turquoise-4"), mk_number_from_int(0x00868b)); 
    hash_table_set(_colour_table_, mk_symbol("cadetblue"), mk_number_from_int(0x5f9ea0)); 
    hash_table_set(_colour_table_, mk_symbol("darkturquoise"), mk_number_from_int(0x00ced1)); 
    hash_table_set(_colour_table_, mk_symbol("azure-1"), mk_number_from_int(0xf0ffff)); 
    hash_table_set(_colour_table_, mk_symbol("azure-2"), mk_number_from_int(0xe0eeee)); 
    hash_table_set(_colour_table_, mk_symbol("azure-3"), mk_number_from_int(0xc1cdcd)); 
    hash_table_set(_colour_table_, mk_symbol("azure-4"), mk_number_from_int(0x838b8b)); 
    hash_table_set(_colour_table_, mk_symbol("lightcyan-1"), mk_number_from_int(0xe0ffff)); 
    hash_table_set(_colour_table_, mk_symbol("lightcyan-2"), mk_number_from_int(0xd1eeee)); 
    hash_table_set(_colour_table_, mk_symbol("lightcyan-3"), mk_number_from_int(0xb4cdcd)); 
    hash_table_set(_colour_table_, mk_symbol("lightcyan-4"), mk_number_from_int(0x7a8b8b)); 
    hash_table_set(_colour_table_, mk_symbol("paleturquoise-1"), mk_number_from_int(0xbbffff)); 
    hash_table_set(_colour_table_, mk_symbol("paleturquoise-2"), mk_number_from_int(0xaeeeee)); 
    hash_table_set(_colour_table_, mk_symbol("paleturquoise-3"), mk_number_from_int(0x96cdcd)); 
    hash_table_set(_colour_table_, mk_symbol("paleturquoise-4"), mk_number_from_int(0x668b8b)); 
    hash_table_set(_colour_table_, mk_symbol("darkslategray"), mk_number_from_int(0x2f4f4f)); 
    hash_table_set(_colour_table_, mk_symbol("darkslategray-1"), mk_number_from_int(0x97ffff)); 
    hash_table_set(_colour_table_, mk_symbol("darkslategray-2"), mk_number_from_int(0x8deeee)); 
    hash_table_set(_colour_table_, mk_symbol("darkslategray-3"), mk_number_from_int(0x79cdcd)); 
    hash_table_set(_colour_table_, mk_symbol("darkslategray-4"), mk_number_from_int(0x528b8b)); 
    hash_table_set(_colour_table_, mk_symbol("cyan"), mk_number_from_int(0x00ffff)); 
    hash_table_set(_colour_table_, mk_symbol("cyan-2"), mk_number_from_int(0x00eeee)); 
    hash_table_set(_colour_table_, mk_symbol("cyan-3"), mk_number_from_int(0x00cdcd)); 
    hash_table_set(_colour_table_, mk_symbol("cyan-4"), mk_number_from_int(0x008b8b)); 
    hash_table_set(_colour_table_, mk_symbol("teal"), mk_number_from_int(0x008080)); 
    hash_table_set(_colour_table_, mk_symbol("mediumturquoise"), mk_number_from_int(0x48d1cc)); 
    hash_table_set(_colour_table_, mk_symbol("lightseagreen"), mk_number_from_int(0x20b2aa)); 
    hash_table_set(_colour_table_, mk_symbol("manganeseblue"), mk_number_from_int(0x03a89e)); 
    hash_table_set(_colour_table_, mk_symbol("turquoise"), mk_number_from_int(0x40e0d0)); 
    hash_table_set(_colour_table_, mk_symbol("coldgrey"), mk_number_from_int(0x808a87)); 
    hash_table_set(_colour_table_, mk_symbol("turquoiseblue"), mk_number_from_int(0x00c78c)); 
    hash_table_set(_colour_table_, mk_symbol("aquamarine-1 "), mk_number_from_int(0x7fffd4)); 
    hash_table_set(_colour_table_, mk_symbol("aquamarine-2"), mk_number_from_int(0x76eec6)); 
    hash_table_set(_colour_table_, mk_symbol("aquamarine-3"), mk_number_from_int(0x66cdaa)); 
    hash_table_set(_colour_table_, mk_symbol("aquamarine-4"), mk_number_from_int(0x458b74)); 
    hash_table_set(_colour_table_, mk_symbol("mediumspringgreen"), mk_number_from_int(0x00fa9a)); 
    hash_table_set(_colour_table_, mk_symbol("mintcream"), mk_number_from_int(0xf5fffa)); 
    hash_table_set(_colour_table_, mk_symbol("springgreen"), mk_number_from_int(0x00ff7f)); 
    hash_table_set(_colour_table_, mk_symbol("springgreen-1"), mk_number_from_int(0x00ee76)); 
    hash_table_set(_colour_table_, mk_symbol("springgreen-2"), mk_number_from_int(0x00cd66)); 
    hash_table_set(_colour_table_, mk_symbol("springgreen-3"), mk_number_from_int(0x008b45)); 
    hash_table_set(_colour_table_, mk_symbol("mediumseagreen"), mk_number_from_int(0x3cb371)); 
    hash_table_set(_colour_table_, mk_symbol("seagreen-1"), mk_number_from_int(0x54ff9f)); 
    hash_table_set(_colour_table_, mk_symbol("seagreen-2"), mk_number_from_int(0x4eee94)); 
    hash_table_set(_colour_table_, mk_symbol("seagreen-3"), mk_number_from_int(0x43cd80)); 
    hash_table_set(_colour_table_, mk_symbol("seagreen-4"), mk_number_from_int(0x2e8b57)); 
    hash_table_set(_colour_table_, mk_symbol("emeraldgreen"), mk_number_from_int(0x00c957)); 
    hash_table_set(_colour_table_, mk_symbol("mint"), mk_number_from_int(0xbdfcc9)); 
    hash_table_set(_colour_table_, mk_symbol("cobaltgreen"), mk_number_from_int(0x3d9140)); 
    hash_table_set(_colour_table_, mk_symbol("honeydew-1"), mk_number_from_int(0xf0fff0)); 
    hash_table_set(_colour_table_, mk_symbol("honeydew-2"), mk_number_from_int(0xe0eee0)); 
    hash_table_set(_colour_table_, mk_symbol("honeydew-3"), mk_number_from_int(0xc1cdc1)); 
    hash_table_set(_colour_table_, mk_symbol("honeydew-4"), mk_number_from_int(0x838b83)); 
    hash_table_set(_colour_table_, mk_symbol("darkseagreen"), mk_number_from_int(0x8fbc8f)); 
    hash_table_set(_colour_table_, mk_symbol("darkseagreen-1"), mk_number_from_int(0xc1ffc1)); 
    hash_table_set(_colour_table_, mk_symbol("darkseagreen-2"), mk_number_from_int(0xb4eeb4)); 
    hash_table_set(_colour_table_, mk_symbol("darkseagreen-3"), mk_number_from_int(0x9bcd9b)); 
    hash_table_set(_colour_table_, mk_symbol("darkseagreen-4"), mk_number_from_int(0x698b69)); 
    hash_table_set(_colour_table_, mk_symbol("palegreen"), mk_number_from_int(0x98fb98)); 
    hash_table_set(_colour_table_, mk_symbol("palegreen-1"), mk_number_from_int(0x9aff9a)); 
    hash_table_set(_colour_table_, mk_symbol("palegreen-2"), mk_number_from_int(0x90ee90)); 
    hash_table_set(_colour_table_, mk_symbol("palegreen-3"), mk_number_from_int(0x7ccd7c)); 
    hash_table_set(_colour_table_, mk_symbol("palegreen-4"), mk_number_from_int(0x548b54)); 
    hash_table_set(_colour_table_, mk_symbol("limegreen"), mk_number_from_int(0x32cd32)); 
    hash_table_set(_colour_table_, mk_symbol("forestgreen"), mk_number_from_int(0x228b22)); 
    hash_table_set(_colour_table_, mk_symbol("green-1"), mk_number_from_int(0x00ff00)); 
    hash_table_set(_colour_table_, mk_symbol("green-2"), mk_number_from_int(0x00ee00)); 
    hash_table_set(_colour_table_, mk_symbol("green-3"), mk_number_from_int(0x00cd00)); 
    hash_table_set(_colour_table_, mk_symbol("green-4"), mk_number_from_int(0x008b00)); 
    hash_table_set(_colour_table_, mk_symbol("green"), mk_number_from_int(0x008000)); 
    hash_table_set(_colour_table_, mk_symbol("darkgreen"), mk_number_from_int(0x006400)); 
    hash_table_set(_colour_table_, mk_symbol("sapgreen"), mk_number_from_int(0x308014)); 
    hash_table_set(_colour_table_, mk_symbol("lawngreen"), mk_number_from_int(0x7cfc00)); 
    hash_table_set(_colour_table_, mk_symbol("chartreuse-1"), mk_number_from_int(0x7fff00)); 
    hash_table_set(_colour_table_, mk_symbol("chartreuse-2"), mk_number_from_int(0x76ee00)); 
    hash_table_set(_colour_table_, mk_symbol("chartreuse-3"), mk_number_from_int(0x66cd00)); 
    hash_table_set(_colour_table_, mk_symbol("chartreuse-4"), mk_number_from_int(0x458b00)); 
    hash_table_set(_colour_table_, mk_symbol("greenyellow"), mk_number_from_int(0xadff2f)); 
    hash_table_set(_colour_table_, mk_symbol("darkolivegreen-1"), mk_number_from_int(0xcaff70));
    hash_table_set(_colour_table_, mk_symbol("darkolivegreen-2"), mk_number_from_int(0xbcee68));
    hash_table_set(_colour_table_, mk_symbol("darkolivegreen-3"), mk_number_from_int(0xa2cd5a));
    hash_table_set(_colour_table_, mk_symbol("darkolivegreen-4"), mk_number_from_int(0x6e8b3d));
    hash_table_set(_colour_table_, mk_symbol("darkolivegreen"), mk_number_from_int(0x556b2f)); 
    hash_table_set(_colour_table_, mk_symbol("olivedrab"), mk_number_from_int(0x6b8e23)); 
    hash_table_set(_colour_table_, mk_symbol("olivedrab-1"), mk_number_from_int(0xc0ff3e)); 
    hash_table_set(_colour_table_, mk_symbol("olivedrab-2"), mk_number_from_int(0xb3ee3a)); 
    hash_table_set(_colour_table_, mk_symbol("olivedrab-3"), mk_number_from_int(0x9acd32)); 
    hash_table_set(_colour_table_, mk_symbol("olivedrab-4"), mk_number_from_int(0x698b22)); 
    hash_table_set(_colour_table_, mk_symbol("ivory-1"), mk_number_from_int(0xfffff0)); 
    hash_table_set(_colour_table_, mk_symbol("ivory-2"), mk_number_from_int(0xeeeee0)); 
    hash_table_set(_colour_table_, mk_symbol("ivory-3"), mk_number_from_int(0xcdcdc1)); 
    hash_table_set(_colour_table_, mk_symbol("ivory-4"), mk_number_from_int(0x8b8b83)); 
    hash_table_set(_colour_table_, mk_symbol("beige"), mk_number_from_int(0xf5f5dc)); 
    hash_table_set(_colour_table_, mk_symbol("lightyellow-1"), mk_number_from_int(0xffffe0)); 
    hash_table_set(_colour_table_, mk_symbol("lightyellow-2"), mk_number_from_int(0xeeeed1)); 
    hash_table_set(_colour_table_, mk_symbol("lightyellow-3"), mk_number_from_int(0xcdcdb4)); 
    hash_table_set(_colour_table_, mk_symbol("lightyellow-4"), mk_number_from_int(0x8b8b7a)); 
    hash_table_set(_colour_table_, mk_symbol("lightgoldenrodyellow"), mk_number_from_int(0xfafad2));
    hash_table_set(_colour_table_, mk_symbol("yellow-1 "), mk_number_from_int(0xffff00)); 
    hash_table_set(_colour_table_, mk_symbol("yellow-2"), mk_number_from_int(0xeeee00)); 
    hash_table_set(_colour_table_, mk_symbol("yellow-3"), mk_number_from_int(0xcdcd00)); 
    hash_table_set(_colour_table_, mk_symbol("yellow-4"), mk_number_from_int(0x8b8b00)); 
    hash_table_set(_colour_table_, mk_symbol("warmgrey"), mk_number_from_int(0x808069)); 
    hash_table_set(_colour_table_, mk_symbol("olive"), mk_number_from_int(0x808000)); 
    hash_table_set(_colour_table_, mk_symbol("darkkhaki"), mk_number_from_int(0xbdb76b)); 
    hash_table_set(_colour_table_, mk_symbol("khaki-1"), mk_number_from_int(0xfff68f)); 
    hash_table_set(_colour_table_, mk_symbol("khaki-2"), mk_number_from_int(0xeee685)); 
    hash_table_set(_colour_table_, mk_symbol("khaki-3"), mk_number_from_int(0xcdc673)); 
    hash_table_set(_colour_table_, mk_symbol("khaki-4"), mk_number_from_int(0x8b864e)); 
    hash_table_set(_colour_table_, mk_symbol("khaki"), mk_number_from_int(0xf0e68c)); 
    hash_table_set(_colour_table_, mk_symbol("palegoldenrod"), mk_number_from_int(0xeee8aa)); 
    hash_table_set(_colour_table_, mk_symbol("lemonchiffon-1"), mk_number_from_int(0xfffacd)); 
    hash_table_set(_colour_table_, mk_symbol("lemonchiffon-2"), mk_number_from_int(0xeee9bf)); 
    hash_table_set(_colour_table_, mk_symbol("lemonchiffon-3"), mk_number_from_int(0xcdc9a5)); 
    hash_table_set(_colour_table_, mk_symbol("lemonchiffon-4"), mk_number_from_int(0x8b8970)); 
    hash_table_set(_colour_table_, mk_symbol("lightgoldenrod-1"), mk_number_from_int(0xffec8b)); 
    hash_table_set(_colour_table_, mk_symbol("lightgoldenrod-2"), mk_number_from_int(0xeedc82));
    hash_table_set(_colour_table_, mk_symbol("lightgoldenrod-3"), mk_number_from_int(0xcdbe70));
    hash_table_set(_colour_table_, mk_symbol("lightgoldenrod-4"), mk_number_from_int(0x8b814c));
    hash_table_set(_colour_table_, mk_symbol("banana"), mk_number_from_int(0xe3cf57)); 
    hash_table_set(_colour_table_, mk_symbol("gold-1"), mk_number_from_int(0xffd700)); 
    hash_table_set(_colour_table_, mk_symbol("gold-2"), mk_number_from_int(0xeec900)); 
    hash_table_set(_colour_table_, mk_symbol("gold-3"), mk_number_from_int(0xcdad00)); 
    hash_table_set(_colour_table_, mk_symbol("gold-4"), mk_number_from_int(0x8b7500)); 
    hash_table_set(_colour_table_, mk_symbol("cornsilk-1"), mk_number_from_int(0xfff8dc)); 
    hash_table_set(_colour_table_, mk_symbol("cornsilk-2"), mk_number_from_int(0xeee8cd)); 
    hash_table_set(_colour_table_, mk_symbol("cornsilk-3"), mk_number_from_int(0xcdc8b1)); 
    hash_table_set(_colour_table_, mk_symbol("cornsilk-4"), mk_number_from_int(0x8b8878)); 
    hash_table_set(_colour_table_, mk_symbol("goldenrod"), mk_number_from_int(0xdaa520)); 
    hash_table_set(_colour_table_, mk_symbol("goldenrod-1"), mk_number_from_int(0xffc125)); 
    hash_table_set(_colour_table_, mk_symbol("goldenrod-2"), mk_number_from_int(0xeeb422)); 
    hash_table_set(_colour_table_, mk_symbol("goldenrod-3"), mk_number_from_int(0xcd9b1d)); 
    hash_table_set(_colour_table_, mk_symbol("goldenrod-4"), mk_number_from_int(0x8b6914)); 
    hash_table_set(_colour_table_, mk_symbol("darkgoldenrod"), mk_number_from_int(0xb8860b)); 
    hash_table_set(_colour_table_, mk_symbol("darkgoldenrod-1"), mk_number_from_int(0xffb90f)); 
    hash_table_set(_colour_table_, mk_symbol("darkgoldenrod-2"), mk_number_from_int(0xeead0e)); 
    hash_table_set(_colour_table_, mk_symbol("darkgoldenrod-3"), mk_number_from_int(0xcd950c)); 
    hash_table_set(_colour_table_, mk_symbol("darkgoldenrod-4"), mk_number_from_int(0x8b6508)); 
    hash_table_set(_colour_table_, mk_symbol("orange-1"), mk_number_from_int(0xffa500)); 
    hash_table_set(_colour_table_, mk_symbol("orange-2"), mk_number_from_int(0xee9a00)); 
    hash_table_set(_colour_table_, mk_symbol("orange-3"), mk_number_from_int(0xcd8500)); 
    hash_table_set(_colour_table_, mk_symbol("orange-4"), mk_number_from_int(0x8b5a00)); 
    hash_table_set(_colour_table_, mk_symbol("floralwhite"), mk_number_from_int(0xfffaf0)); 
    hash_table_set(_colour_table_, mk_symbol("oldlace"), mk_number_from_int(0xfdf5e6)); 
    hash_table_set(_colour_table_, mk_symbol("wheat"), mk_number_from_int(0xf5deb3)); 
    hash_table_set(_colour_table_, mk_symbol("wheat-1"), mk_number_from_int(0xffe7ba)); 
    hash_table_set(_colour_table_, mk_symbol("wheat-2"), mk_number_from_int(0xeed8ae)); 
    hash_table_set(_colour_table_, mk_symbol("wheat-3"), mk_number_from_int(0xcdba96)); 
    hash_table_set(_colour_table_, mk_symbol("wheat-4"), mk_number_from_int(0x8b7e66)); 
    hash_table_set(_colour_table_, mk_symbol("moccasin"), mk_number_from_int(0xffe4b5)); 
    hash_table_set(_colour_table_, mk_symbol("papayawhip"), mk_number_from_int(0xffefd5)); 
    hash_table_set(_colour_table_, mk_symbol("blanchedalmond"), mk_number_from_int(0xffebcd)); 
    hash_table_set(_colour_table_, mk_symbol("navajowhite-1"), mk_number_from_int(0xffdead)); 
    hash_table_set(_colour_table_, mk_symbol("navajowhite-2"), mk_number_from_int(0xeecfa1)); 
    hash_table_set(_colour_table_, mk_symbol("navajowhite-3"), mk_number_from_int(0xcdb38b)); 
    hash_table_set(_colour_table_, mk_symbol("navajowhite-4"), mk_number_from_int(0x8b795e)); 
    hash_table_set(_colour_table_, mk_symbol("eggshell"), mk_number_from_int(0xfce6c9)); 
    hash_table_set(_colour_table_, mk_symbol("tan"), mk_number_from_int(0xd2b48c)); 
    hash_table_set(_colour_table_, mk_symbol("brick"), mk_number_from_int(0x9c661f)); 
    hash_table_set(_colour_table_, mk_symbol("cadmiumyellow"), mk_number_from_int(0xff9912)); 
    hash_table_set(_colour_table_, mk_symbol("antiquewhite"), mk_number_from_int(0xfaebd7)); 
    hash_table_set(_colour_table_, mk_symbol("antiquewhite-1"), mk_number_from_int(0xffefdb)); 
    hash_table_set(_colour_table_, mk_symbol("antiquewhite-2"), mk_number_from_int(0xeedfcc)); 
    hash_table_set(_colour_table_, mk_symbol("antiquewhite-3"), mk_number_from_int(0xcdc0b0)); 
    hash_table_set(_colour_table_, mk_symbol("antiquewhite-4"), mk_number_from_int(0x8b8378)); 
    hash_table_set(_colour_table_, mk_symbol("burlywood"), mk_number_from_int(0xdeb887)); 
    hash_table_set(_colour_table_, mk_symbol("burlywood-1"), mk_number_from_int(0xffd39b)); 
    hash_table_set(_colour_table_, mk_symbol("burlywood-2"), mk_number_from_int(0xeec591)); 
    hash_table_set(_colour_table_, mk_symbol("burlywood-3"), mk_number_from_int(0xcdaa7d)); 
    hash_table_set(_colour_table_, mk_symbol("burlywood-4"), mk_number_from_int(0x8b7355)); 
    hash_table_set(_colour_table_, mk_symbol("bisque-1"), mk_number_from_int(0xffe4c4)); 
    hash_table_set(_colour_table_, mk_symbol("bisque-2"), mk_number_from_int(0xeed5b7)); 
    hash_table_set(_colour_table_, mk_symbol("bisque-3"), mk_number_from_int(0xcdb79e)); 
    hash_table_set(_colour_table_, mk_symbol("bisque-4"), mk_number_from_int(0x8b7d6b)); 
    hash_table_set(_colour_table_, mk_symbol("melon"), mk_number_from_int(0xe3a869)); 
    hash_table_set(_colour_table_, mk_symbol("carrot"), mk_number_from_int(0xed9121)); 
    hash_table_set(_colour_table_, mk_symbol("darkorange"), mk_number_from_int(0xff8c00)); 
    hash_table_set(_colour_table_, mk_symbol("darkorange-1"), mk_number_from_int(0xff7f00)); 
    hash_table_set(_colour_table_, mk_symbol("darkorange-2"), mk_number_from_int(0xee7600)); 
    hash_table_set(_colour_table_, mk_symbol("darkorange-3"), mk_number_from_int(0xcd6600)); 
    hash_table_set(_colour_table_, mk_symbol("darkorange-4"), mk_number_from_int(0x8b4500)); 
    hash_table_set(_colour_table_, mk_symbol("orange"), mk_number_from_int(0xff8000)); 
    hash_table_set(_colour_table_, mk_symbol("tan-1"), mk_number_from_int(0xffa54f)); 
    hash_table_set(_colour_table_, mk_symbol("tan-2"), mk_number_from_int(0xee9a49)); 
    hash_table_set(_colour_table_, mk_symbol("tan-3"), mk_number_from_int(0xcd853f)); 
    hash_table_set(_colour_table_, mk_symbol("tan-4"), mk_number_from_int(0x8b5a2b)); 
    hash_table_set(_colour_table_, mk_symbol("linen"), mk_number_from_int(0xfaf0e6)); 
    hash_table_set(_colour_table_, mk_symbol("peachpuff-1"), mk_number_from_int(0xffdab9)); 
    hash_table_set(_colour_table_, mk_symbol("peachpuff-2"), mk_number_from_int(0xeecbad)); 
    hash_table_set(_colour_table_, mk_symbol("peachpuff-3"), mk_number_from_int(0xcdaf95)); 
    hash_table_set(_colour_table_, mk_symbol("peachpuff-4"), mk_number_from_int(0x8b7765)); 
    hash_table_set(_colour_table_, mk_symbol("seashell-1"), mk_number_from_int(0xfff5ee)); 
    hash_table_set(_colour_table_, mk_symbol("seashell-2"), mk_number_from_int(0xeee5de)); 
    hash_table_set(_colour_table_, mk_symbol("seashell-3"), mk_number_from_int(0xcdc5bf)); 
    hash_table_set(_colour_table_, mk_symbol("seashell-4"), mk_number_from_int(0x8b8682)); 
    hash_table_set(_colour_table_, mk_symbol("sandybrown"), mk_number_from_int(0xf4a460)); 
    hash_table_set(_colour_table_, mk_symbol("rawsienna"), mk_number_from_int(0xc76114)); 
    hash_table_set(_colour_table_, mk_symbol("chocolate"), mk_number_from_int(0xd2691e)); 
    hash_table_set(_colour_table_, mk_symbol("chocolate-1"), mk_number_from_int(0xff7f24)); 
    hash_table_set(_colour_table_, mk_symbol("chocolate-2"), mk_number_from_int(0xee7621)); 
    hash_table_set(_colour_table_, mk_symbol("chocolate-3"), mk_number_from_int(0xcd661d)); 
    hash_table_set(_colour_table_, mk_symbol("chocolate-4"), mk_number_from_int(0x8b4513)); 
    hash_table_set(_colour_table_, mk_symbol("ivoryblack"), mk_number_from_int(0x292421)); 
    hash_table_set(_colour_table_, mk_symbol("flesh"), mk_number_from_int(0xff7d40)); 
    hash_table_set(_colour_table_, mk_symbol("cadmiumorange"), mk_number_from_int(0xff6103)); 
    hash_table_set(_colour_table_, mk_symbol("burntsienna"), mk_number_from_int(0x8a360f)); 
    hash_table_set(_colour_table_, mk_symbol("sienna"), mk_number_from_int(0xa0522d)); 
    hash_table_set(_colour_table_, mk_symbol("sienna-1"), mk_number_from_int(0xff8247)); 
    hash_table_set(_colour_table_, mk_symbol("sienna-2"), mk_number_from_int(0xee7942)); 
    hash_table_set(_colour_table_, mk_symbol("sienna-3"), mk_number_from_int(0xcd6839)); 
    hash_table_set(_colour_table_, mk_symbol("sienna-4"), mk_number_from_int(0x8b4726)); 
    hash_table_set(_colour_table_, mk_symbol("lightsalmon-1 "), mk_number_from_int(0xffa07a)); 
    hash_table_set(_colour_table_, mk_symbol("lightsalmon-2"), mk_number_from_int(0xee9572)); 
    hash_table_set(_colour_table_, mk_symbol("lightsalmon-3"), mk_number_from_int(0xcd8162)); 
    hash_table_set(_colour_table_, mk_symbol("lightsalmon-4"), mk_number_from_int(0x8b5742)); 
    hash_table_set(_colour_table_, mk_symbol("coral"), mk_number_from_int(0xff7f50)); 
    hash_table_set(_colour_table_, mk_symbol("orangered-1"), mk_number_from_int(0xff4500)); 
    hash_table_set(_colour_table_, mk_symbol("orangered-2"), mk_number_from_int(0xee4000)); 
    hash_table_set(_colour_table_, mk_symbol("orangered-3"), mk_number_from_int(0xcd3700)); 
    hash_table_set(_colour_table_, mk_symbol("orangered-4"), mk_number_from_int(0x8b2500)); 
    hash_table_set(_colour_table_, mk_symbol("sepia"), mk_number_from_int(0x5e2612)); 
    hash_table_set(_colour_table_, mk_symbol("darksalmon"), mk_number_from_int(0xe9967a)); 
    hash_table_set(_colour_table_, mk_symbol("salmon-1"), mk_number_from_int(0xff8c69)); 
    hash_table_set(_colour_table_, mk_symbol("salmon-2"), mk_number_from_int(0xee8262)); 
    hash_table_set(_colour_table_, mk_symbol("salmon-3"), mk_number_from_int(0xcd7054)); 
    hash_table_set(_colour_table_, mk_symbol("salmon-4"), mk_number_from_int(0x8b4c39)); 
    hash_table_set(_colour_table_, mk_symbol("coral-1"), mk_number_from_int(0xff7256)); 
    hash_table_set(_colour_table_, mk_symbol("coral-2"), mk_number_from_int(0xee6a50)); 
    hash_table_set(_colour_table_, mk_symbol("coral-3"), mk_number_from_int(0xcd5b45)); 
    hash_table_set(_colour_table_, mk_symbol("coral-4"), mk_number_from_int(0x8b3e2f)); 
    hash_table_set(_colour_table_, mk_symbol("burntumber"), mk_number_from_int(0x8a3324)); 
    hash_table_set(_colour_table_, mk_symbol("tomato-1"), mk_number_from_int(0xff6347)); 
    hash_table_set(_colour_table_, mk_symbol("tomato-2"), mk_number_from_int(0xee5c42)); 
    hash_table_set(_colour_table_, mk_symbol("tomato-3"), mk_number_from_int(0xcd4f39)); 
    hash_table_set(_colour_table_, mk_symbol("tomato-4"), mk_number_from_int(0x8b3626)); 
    hash_table_set(_colour_table_, mk_symbol("salmon"), mk_number_from_int(0xfa8072)); 
    hash_table_set(_colour_table_, mk_symbol("mistyrose-1 "), mk_number_from_int(0xffe4e1)); 
    hash_table_set(_colour_table_, mk_symbol("mistyrose-2"), mk_number_from_int(0xeed5d2)); 
    hash_table_set(_colour_table_, mk_symbol("mistyrose-3"), mk_number_from_int(0xcdb7b5)); 
    hash_table_set(_colour_table_, mk_symbol("mistyrose-4"), mk_number_from_int(0x8b7d7b)); 
    hash_table_set(_colour_table_, mk_symbol("snow-1"), mk_number_from_int(0xfffafa)); 
    hash_table_set(_colour_table_, mk_symbol("snow-2"), mk_number_from_int(0xeee9e9)); 
    hash_table_set(_colour_table_, mk_symbol("snow-3"), mk_number_from_int(0xcdc9c9)); 
    hash_table_set(_colour_table_, mk_symbol("snow-4"), mk_number_from_int(0x8b8989)); 
    hash_table_set(_colour_table_, mk_symbol("rosybrown"), mk_number_from_int(0xbc8f8f)); 
    hash_table_set(_colour_table_, mk_symbol("rosybrown-1"), mk_number_from_int(0xffc1c1)); 
    hash_table_set(_colour_table_, mk_symbol("rosybrown-2"), mk_number_from_int(0xeeb4b4)); 
    hash_table_set(_colour_table_, mk_symbol("rosybrown-3"), mk_number_from_int(0xcd9b9b)); 
    hash_table_set(_colour_table_, mk_symbol("rosybrown-4"), mk_number_from_int(0x8b6969)); 
    hash_table_set(_colour_table_, mk_symbol("lightcoral"), mk_number_from_int(0xf08080)); 
    hash_table_set(_colour_table_, mk_symbol("indianred"), mk_number_from_int(0xcd5c5c)); 
    hash_table_set(_colour_table_, mk_symbol("indianred-1"), mk_number_from_int(0xff6a6a)); 
    hash_table_set(_colour_table_, mk_symbol("indianred-2"), mk_number_from_int(0xee6363)); 
    hash_table_set(_colour_table_, mk_symbol("indianred-4"), mk_number_from_int(0x8b3a3a)); 
    hash_table_set(_colour_table_, mk_symbol("indianred-3"), mk_number_from_int(0xcd5555)); 
    hash_table_set(_colour_table_, mk_symbol("brown"), mk_number_from_int(0xa52a2a)); 
    hash_table_set(_colour_table_, mk_symbol("brown-1"), mk_number_from_int(0xff4040)); 
    hash_table_set(_colour_table_, mk_symbol("brown-2"), mk_number_from_int(0xee3b3b)); 
    hash_table_set(_colour_table_, mk_symbol("brown-3"), mk_number_from_int(0xcd3333)); 
    hash_table_set(_colour_table_, mk_symbol("brown-4"), mk_number_from_int(0x8b2323)); 
    hash_table_set(_colour_table_, mk_symbol("firebrick"), mk_number_from_int(0xb22222)); 
    hash_table_set(_colour_table_, mk_symbol("firebrick-1"), mk_number_from_int(0xff3030)); 
    hash_table_set(_colour_table_, mk_symbol("firebrick-2"), mk_number_from_int(0xee2c2c)); 
    hash_table_set(_colour_table_, mk_symbol("firebrick-3"), mk_number_from_int(0xcd2626)); 
    hash_table_set(_colour_table_, mk_symbol("firebrick-4"), mk_number_from_int(0x8b1a1a)); 
    hash_table_set(_colour_table_, mk_symbol("red-1"), mk_number_from_int(0xff0000)); 
    hash_table_set(_colour_table_, mk_symbol("red-2"), mk_number_from_int(0xee0000)); 
    hash_table_set(_colour_table_, mk_symbol("red-3"), mk_number_from_int(0xcd0000)); 
    hash_table_set(_colour_table_, mk_symbol("red-4"), mk_number_from_int(0x8b0000)); 
    hash_table_set(_colour_table_, mk_symbol("maroon"), mk_number_from_int(0x800000)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-beet"), mk_number_from_int(0x8e388e)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-slateblue"), mk_number_from_int(0x7171c6)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-lightblue"), mk_number_from_int(0x7d9ec0)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-teal"), mk_number_from_int(0x388e8e)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-chartreuse"), mk_number_from_int(0x71c671)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-olivedrab"), mk_number_from_int(0x8e8e38)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-brightgray"), mk_number_from_int(0xc5c1aa)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-salmon"), mk_number_from_int(0xc67171)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-darkgray"), mk_number_from_int(0x555555)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-12"), mk_number_from_int(0x1e1e1e)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-16"), mk_number_from_int(0x282828)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-32"), mk_number_from_int(0x515151)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-36"), mk_number_from_int(0x5b5b5b)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-52"), mk_number_from_int(0x848484)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-56"), mk_number_from_int(0x8e8e8e)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-lightgray"), mk_number_from_int(0xaaaaaa)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-72"), mk_number_from_int(0xb7b7b7)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-76"), mk_number_from_int(0xc1c1c1)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-92"), mk_number_from_int(0xeaeaea)); 
    hash_table_set(_colour_table_, mk_symbol("sgi-gray-96"), mk_number_from_int(0xf4f4f4)); 
    hash_table_set(_colour_table_, mk_symbol("white"), mk_number_from_int(0xffffff)); 
    hash_table_set(_colour_table_, mk_symbol("gainsboro"), mk_number_from_int(0xdcdcdc)); 
    hash_table_set(_colour_table_, mk_symbol("lightgrey"), mk_number_from_int(0xd3d3d3)); 
    hash_table_set(_colour_table_, mk_symbol("silver"), mk_number_from_int(0xc0c0c0)); 
    hash_table_set(_colour_table_, mk_symbol("darkgray"), mk_number_from_int(0xa9a9a9)); 
    hash_table_set(_colour_table_, mk_symbol("gray"), mk_number_from_int(0x808080)); 
    hash_table_set(_colour_table_, mk_symbol("black"), mk_number_from_int(0x000000)); 
    hash_table_set(_colour_table_, mk_symbol("gray-99"), mk_number_from_int(0xfcfcfc)); 
    hash_table_set(_colour_table_, mk_symbol("gray-98"), mk_number_from_int(0xfafafa)); 
    hash_table_set(_colour_table_, mk_symbol("gray-97"), mk_number_from_int(0xf7f7f7)); 
    hash_table_set(_colour_table_, mk_symbol("white-smoke"), mk_number_from_int(0xf5f5f5)); 
    hash_table_set(_colour_table_, mk_symbol("gray-95"), mk_number_from_int(0xf2f2f2)); 
    hash_table_set(_colour_table_, mk_symbol("gray-94"), mk_number_from_int(0xf0f0f0)); 
    hash_table_set(_colour_table_, mk_symbol("gray-93"), mk_number_from_int(0xededed)); 
    hash_table_set(_colour_table_, mk_symbol("gray-92"), mk_number_from_int(0xebebeb)); 
    hash_table_set(_colour_table_, mk_symbol("gray-91"), mk_number_from_int(0xe8e8e8)); 
    hash_table_set(_colour_table_, mk_symbol("gray-90"), mk_number_from_int(0xe5e5e5)); 
    hash_table_set(_colour_table_, mk_symbol("gray-89"), mk_number_from_int(0xe3e3e3)); 
    hash_table_set(_colour_table_, mk_symbol("gray-88"), mk_number_from_int(0xe0e0e0)); 
    hash_table_set(_colour_table_, mk_symbol("gray-87"), mk_number_from_int(0xdedede)); 
    hash_table_set(_colour_table_, mk_symbol("gray-86"), mk_number_from_int(0xdbdbdb)); 
    hash_table_set(_colour_table_, mk_symbol("gray-85"), mk_number_from_int(0xd9d9d9)); 
    hash_table_set(_colour_table_, mk_symbol("gray-84"), mk_number_from_int(0xd6d6d6)); 
    hash_table_set(_colour_table_, mk_symbol("gray-83"), mk_number_from_int(0xd4d4d4)); 
    hash_table_set(_colour_table_, mk_symbol("gray-82"), mk_number_from_int(0xd1d1d1)); 
    hash_table_set(_colour_table_, mk_symbol("gray-81"), mk_number_from_int(0xcfcfcf)); 
    hash_table_set(_colour_table_, mk_symbol("gray-80"), mk_number_from_int(0xcccccc)); 
    hash_table_set(_colour_table_, mk_symbol("gray-79"), mk_number_from_int(0xc9c9c9)); 
    hash_table_set(_colour_table_, mk_symbol("gray-78"), mk_number_from_int(0xc7c7c7)); 
    hash_table_set(_colour_table_, mk_symbol("gray-77"), mk_number_from_int(0xc4c4c4)); 
    hash_table_set(_colour_table_, mk_symbol("gray-76"), mk_number_from_int(0xc2c2c2)); 
    hash_table_set(_colour_table_, mk_symbol("gray-75"), mk_number_from_int(0xbfbfbf)); 
    hash_table_set(_colour_table_, mk_symbol("gray-74"), mk_number_from_int(0xbdbdbd)); 
    hash_table_set(_colour_table_, mk_symbol("gray-73"), mk_number_from_int(0xbababa)); 
    hash_table_set(_colour_table_, mk_symbol("gray-72"), mk_number_from_int(0xb8b8b8)); 
    hash_table_set(_colour_table_, mk_symbol("gray-71"), mk_number_from_int(0xb5b5b5)); 
    hash_table_set(_colour_table_, mk_symbol("gray-70"), mk_number_from_int(0xb3b3b3)); 
    hash_table_set(_colour_table_, mk_symbol("gray-69"), mk_number_from_int(0xb0b0b0)); 
    hash_table_set(_colour_table_, mk_symbol("gray-68"), mk_number_from_int(0xadadad)); 
    hash_table_set(_colour_table_, mk_symbol("gray-67"), mk_number_from_int(0xababab)); 
    hash_table_set(_colour_table_, mk_symbol("gray-66"), mk_number_from_int(0xa8a8a8)); 
    hash_table_set(_colour_table_, mk_symbol("gray-65"), mk_number_from_int(0xa6a6a6)); 
    hash_table_set(_colour_table_, mk_symbol("gray-64"), mk_number_from_int(0xa3a3a3)); 
    hash_table_set(_colour_table_, mk_symbol("gray-63"), mk_number_from_int(0xa1a1a1)); 
    hash_table_set(_colour_table_, mk_symbol("gray-62"), mk_number_from_int(0x9e9e9e)); 
    hash_table_set(_colour_table_, mk_symbol("gray-61"), mk_number_from_int(0x9c9c9c)); 
    hash_table_set(_colour_table_, mk_symbol("gray-60"), mk_number_from_int(0x999999)); 
    hash_table_set(_colour_table_, mk_symbol("gray-59"), mk_number_from_int(0x969696)); 
    hash_table_set(_colour_table_, mk_symbol("gray-58"), mk_number_from_int(0x949494)); 
    hash_table_set(_colour_table_, mk_symbol("gray-57"), mk_number_from_int(0x919191)); 
    hash_table_set(_colour_table_, mk_symbol("gray-56"), mk_number_from_int(0x8f8f8f)); 
    hash_table_set(_colour_table_, mk_symbol("gray-55"), mk_number_from_int(0x8c8c8c)); 
    hash_table_set(_colour_table_, mk_symbol("gray-54"), mk_number_from_int(0x8a8a8a)); 
    hash_table_set(_colour_table_, mk_symbol("gray-53"), mk_number_from_int(0x878787)); 
    hash_table_set(_colour_table_, mk_symbol("gray-52"), mk_number_from_int(0x858585)); 
    hash_table_set(_colour_table_, mk_symbol("gray-51"), mk_number_from_int(0x828282)); 
    hash_table_set(_colour_table_, mk_symbol("gray-50"), mk_number_from_int(0x7f7f7f)); 
    hash_table_set(_colour_table_, mk_symbol("gray-49"), mk_number_from_int(0x7d7d7d)); 
    hash_table_set(_colour_table_, mk_symbol("gray-48"), mk_number_from_int(0x7a7a7a)); 
    hash_table_set(_colour_table_, mk_symbol("gray-47"), mk_number_from_int(0x787878)); 
    hash_table_set(_colour_table_, mk_symbol("gray-46"), mk_number_from_int(0x757575)); 
    hash_table_set(_colour_table_, mk_symbol("gray-45"), mk_number_from_int(0x737373)); 
    hash_table_set(_colour_table_, mk_symbol("gray-44"), mk_number_from_int(0x707070)); 
    hash_table_set(_colour_table_, mk_symbol("gray-43"), mk_number_from_int(0x6e6e6e)); 
    hash_table_set(_colour_table_, mk_symbol("gray-42"), mk_number_from_int(0x6b6b6b)); 
    hash_table_set(_colour_table_, mk_symbol("dimgray "), mk_number_from_int(0x696969)); 
    hash_table_set(_colour_table_, mk_symbol("gray-40"), mk_number_from_int(0x666666)); 
    hash_table_set(_colour_table_, mk_symbol("gray-39"), mk_number_from_int(0x636363)); 
    hash_table_set(_colour_table_, mk_symbol("gray-38"), mk_number_from_int(0x616161)); 
    hash_table_set(_colour_table_, mk_symbol("gray-37"), mk_number_from_int(0x5e5e5e)); 
    hash_table_set(_colour_table_, mk_symbol("gray-36"), mk_number_from_int(0x5c5c5c)); 
    hash_table_set(_colour_table_, mk_symbol("gray-35"), mk_number_from_int(0x595959)); 
    hash_table_set(_colour_table_, mk_symbol("gray-34"), mk_number_from_int(0x575757)); 
    hash_table_set(_colour_table_, mk_symbol("gray-33"), mk_number_from_int(0x545454)); 
    hash_table_set(_colour_table_, mk_symbol("gray-32"), mk_number_from_int(0x525252)); 
    hash_table_set(_colour_table_, mk_symbol("gray-31"), mk_number_from_int(0x4f4f4f)); 
    hash_table_set(_colour_table_, mk_symbol("gray-30"), mk_number_from_int(0x4d4d4d)); 
    hash_table_set(_colour_table_, mk_symbol("gray-29"), mk_number_from_int(0x4a4a4a)); 
    hash_table_set(_colour_table_, mk_symbol("gray-28"), mk_number_from_int(0x474747)); 
    hash_table_set(_colour_table_, mk_symbol("gray-27"), mk_number_from_int(0x454545)); 
    hash_table_set(_colour_table_, mk_symbol("gray-26"), mk_number_from_int(0x424242)); 
    hash_table_set(_colour_table_, mk_symbol("gray-25"), mk_number_from_int(0x404040)); 
    hash_table_set(_colour_table_, mk_symbol("gray-24"), mk_number_from_int(0x3d3d3d)); 
    hash_table_set(_colour_table_, mk_symbol("gray-23"), mk_number_from_int(0x3b3b3b)); 
    hash_table_set(_colour_table_, mk_symbol("gray-22"), mk_number_from_int(0x383838)); 
    hash_table_set(_colour_table_, mk_symbol("gray-21"), mk_number_from_int(0x363636)); 
    hash_table_set(_colour_table_, mk_symbol("gray-20"), mk_number_from_int(0x333333)); 
    hash_table_set(_colour_table_, mk_symbol("gray-19"), mk_number_from_int(0x303030)); 
    hash_table_set(_colour_table_, mk_symbol("gray-18"), mk_number_from_int(0x2e2e2e)); 
    hash_table_set(_colour_table_, mk_symbol("gray-17"), mk_number_from_int(0x2b2b2b)); 
    hash_table_set(_colour_table_, mk_symbol("gray-16"), mk_number_from_int(0x292929)); 
    hash_table_set(_colour_table_, mk_symbol("gray-15"), mk_number_from_int(0x262626)); 
    hash_table_set(_colour_table_, mk_symbol("gray-14"), mk_number_from_int(0x242424)); 
    hash_table_set(_colour_table_, mk_symbol("gray-13"), mk_number_from_int(0x212121)); 
    hash_table_set(_colour_table_, mk_symbol("gray-12"), mk_number_from_int(0x1f1f1f)); 
    hash_table_set(_colour_table_, mk_symbol("gray-11"), mk_number_from_int(0x1c1c1c)); 
    hash_table_set(_colour_table_, mk_symbol("gray-10"), mk_number_from_int(0x1a1a1a)); 
    hash_table_set(_colour_table_, mk_symbol("gray-9"), mk_number_from_int(0x171717)); 
    hash_table_set(_colour_table_, mk_symbol("gray-8"), mk_number_from_int(0x141414)); 
    hash_table_set(_colour_table_, mk_symbol("gray-7"), mk_number_from_int(0x121212)); 
    hash_table_set(_colour_table_, mk_symbol("gray-6"), mk_number_from_int(0x0f0f0f)); 
    hash_table_set(_colour_table_, mk_symbol("gray-5"), mk_number_from_int(0x0d0d0d)); 
    hash_table_set(_colour_table_, mk_symbol("gray-4"), mk_number_from_int(0x0a0a0a)); 
    hash_table_set(_colour_table_, mk_symbol("gray-3"), mk_number_from_int(0x080808)); 
    hash_table_set(_colour_table_, mk_symbol("gray-2"), mk_number_from_int(0x050505)); 
    hash_table_set(_colour_table_, mk_symbol("gray-1"), mk_number_from_int(0x030303));

    /* mapping from X key numbers to symbols */
    _key_table_ = mk_hash_table(is_number_equal, number_hash);
    hash_table_set(_key_table_, mk_number_from_int(0x0020), mk_symbol("xk-space"));
    hash_table_set(_key_table_, mk_number_from_int(0x0021), mk_symbol("xk-exclam"));
    hash_table_set(_key_table_, mk_number_from_int(0x0022), mk_symbol("xk-quotedbl"));
    hash_table_set(_key_table_, mk_number_from_int(0x0023), mk_symbol("xk-numbersign"));                  
    hash_table_set(_key_table_, mk_number_from_int(0x0024), mk_symbol("xk-dollar"));                    
    hash_table_set(_key_table_, mk_number_from_int(0x0025), mk_symbol("xk-percent"));
    hash_table_set(_key_table_, mk_number_from_int(0x0026), mk_symbol("xk-ampersand"));
    hash_table_set(_key_table_, mk_number_from_int(0x0027), mk_symbol("xk-apostrophe"));
    hash_table_set(_key_table_, mk_number_from_int(0x0027), mk_symbol("xk-quoteright"));
    hash_table_set(_key_table_, mk_number_from_int(0x0028), mk_symbol("xk-parenleft"));
    hash_table_set(_key_table_, mk_number_from_int(0x0029), mk_symbol("xk-parenright"));
    hash_table_set(_key_table_, mk_number_from_int(0x002a), mk_symbol("xk-asterisk"));
    hash_table_set(_key_table_, mk_number_from_int(0x002b), mk_symbol("xk-plus"));
    hash_table_set(_key_table_, mk_number_from_int(0x002c), mk_symbol("xk-comma"));
    hash_table_set(_key_table_, mk_number_from_int(0x002d), mk_symbol("xk-minus"));
    hash_table_set(_key_table_, mk_number_from_int(0x002e), mk_symbol("xk-period"));
    hash_table_set(_key_table_, mk_number_from_int(0x002f), mk_symbol("xk-slash"));
    hash_table_set(_key_table_, mk_number_from_int(0x0030), mk_symbol("xk-0"));
    hash_table_set(_key_table_, mk_number_from_int(0x0031), mk_symbol("xk-1"));
    hash_table_set(_key_table_, mk_number_from_int(0x0032), mk_symbol("xk-2"));
    hash_table_set(_key_table_, mk_number_from_int(0x0033), mk_symbol("xk-3"));
    hash_table_set(_key_table_, mk_number_from_int(0x0034), mk_symbol("xk-4"));
    hash_table_set(_key_table_, mk_number_from_int(0x0035), mk_symbol("xk-5"));
    hash_table_set(_key_table_, mk_number_from_int(0x0036), mk_symbol("xk-6"));
    hash_table_set(_key_table_, mk_number_from_int(0x0037), mk_symbol("xk-7"));
    hash_table_set(_key_table_, mk_number_from_int(0x0038), mk_symbol("xk-8"));
    hash_table_set(_key_table_, mk_number_from_int(0x0039), mk_symbol("xk-9"));
    hash_table_set(_key_table_, mk_number_from_int(0x003a), mk_symbol("xk-colon"));
    hash_table_set(_key_table_, mk_number_from_int(0x003b), mk_symbol("xk-semicolon"));
    hash_table_set(_key_table_, mk_number_from_int(0x003c), mk_symbol("xk-less"));
    hash_table_set(_key_table_, mk_number_from_int(0x003d), mk_symbol("xk-equal"));
    hash_table_set(_key_table_, mk_number_from_int(0x003e), mk_symbol("xk-greater"));
    hash_table_set(_key_table_, mk_number_from_int(0x003f), mk_symbol("xk-question"));
    hash_table_set(_key_table_, mk_number_from_int(0x0040), mk_symbol("xk-at"));
    hash_table_set(_key_table_, mk_number_from_int(0x0041), mk_symbol("xk-A"));
    hash_table_set(_key_table_, mk_number_from_int(0x0042), mk_symbol("xk-B"));
    hash_table_set(_key_table_, mk_number_from_int(0x0043), mk_symbol("xk-C"));
    hash_table_set(_key_table_, mk_number_from_int(0x0044), mk_symbol("xk-D"));
    hash_table_set(_key_table_, mk_number_from_int(0x0045), mk_symbol("xk-E"));
    hash_table_set(_key_table_, mk_number_from_int(0x0046), mk_symbol("xk-F"));
    hash_table_set(_key_table_, mk_number_from_int(0x0047), mk_symbol("xk-G"));
    hash_table_set(_key_table_, mk_number_from_int(0x0048), mk_symbol("xk-H"));
    hash_table_set(_key_table_, mk_number_from_int(0x0049), mk_symbol("xk-I"));
    hash_table_set(_key_table_, mk_number_from_int(0x004a), mk_symbol("xk-J"));
    hash_table_set(_key_table_, mk_number_from_int(0x004b), mk_symbol("xk-K"));
    hash_table_set(_key_table_, mk_number_from_int(0x004c), mk_symbol("xk-L"));
    hash_table_set(_key_table_, mk_number_from_int(0x004d), mk_symbol("xk-M"));
    hash_table_set(_key_table_, mk_number_from_int(0x004e), mk_symbol("xk-N"));
    hash_table_set(_key_table_, mk_number_from_int(0x004f), mk_symbol("xk-O"));
    hash_table_set(_key_table_, mk_number_from_int(0x0050), mk_symbol("xk-P"));
    hash_table_set(_key_table_, mk_number_from_int(0x0051), mk_symbol("xk-Q"));
    hash_table_set(_key_table_, mk_number_from_int(0x0052), mk_symbol("xk-R"));
    hash_table_set(_key_table_, mk_number_from_int(0x0053), mk_symbol("xk-S"));
    hash_table_set(_key_table_, mk_number_from_int(0x0054), mk_symbol("xk-T"));
    hash_table_set(_key_table_, mk_number_from_int(0x0055), mk_symbol("xk-U"));
    hash_table_set(_key_table_, mk_number_from_int(0x0056), mk_symbol("xk-V"));
    hash_table_set(_key_table_, mk_number_from_int(0x0057), mk_symbol("xk-W"));
    hash_table_set(_key_table_, mk_number_from_int(0x0058), mk_symbol("xk-X"));
    hash_table_set(_key_table_, mk_number_from_int(0x0059), mk_symbol("xk-Y"));
    hash_table_set(_key_table_, mk_number_from_int(0x005a), mk_symbol("xk-Z"));
    hash_table_set(_key_table_, mk_number_from_int(0x005b), mk_symbol("xk-bracketleft"));
    hash_table_set(_key_table_, mk_number_from_int(0x005c), mk_symbol("xk-backslash"));
    hash_table_set(_key_table_, mk_number_from_int(0x005d), mk_symbol("xk-bracketright"));
    hash_table_set(_key_table_, mk_number_from_int(0x005e), mk_symbol("xk-asciicircum"));
    hash_table_set(_key_table_, mk_number_from_int(0x005f), mk_symbol("xk-underscore"));
    hash_table_set(_key_table_, mk_number_from_int(0x0060), mk_symbol("xk-grave"));
    hash_table_set(_key_table_, mk_number_from_int(0x0060), mk_symbol("xk-quoteleft"));
    hash_table_set(_key_table_, mk_number_from_int(0x0061), mk_symbol("xk-a"));
    hash_table_set(_key_table_, mk_number_from_int(0x0062), mk_symbol("xk-b"));
    hash_table_set(_key_table_, mk_number_from_int(0x0063), mk_symbol("xk-c"));
    hash_table_set(_key_table_, mk_number_from_int(0x0064), mk_symbol("xk-d"));
    hash_table_set(_key_table_, mk_number_from_int(0x0065), mk_symbol("xk-e"));
    hash_table_set(_key_table_, mk_number_from_int(0x0066), mk_symbol("xk-f"));
    hash_table_set(_key_table_, mk_number_from_int(0x0067), mk_symbol("xk-g"));
    hash_table_set(_key_table_, mk_number_from_int(0x0068), mk_symbol("xk-h"));
    hash_table_set(_key_table_, mk_number_from_int(0x0069), mk_symbol("xk-i"));
    hash_table_set(_key_table_, mk_number_from_int(0x006a), mk_symbol("xk-j"));
    hash_table_set(_key_table_, mk_number_from_int(0x006b), mk_symbol("xk-k"));
    hash_table_set(_key_table_, mk_number_from_int(0x006c), mk_symbol("xk-l"));
    hash_table_set(_key_table_, mk_number_from_int(0x006d), mk_symbol("xk-m"));
    hash_table_set(_key_table_, mk_number_from_int(0x006e), mk_symbol("xk-n"));
    hash_table_set(_key_table_, mk_number_from_int(0x006f), mk_symbol("xk-o"));
    hash_table_set(_key_table_, mk_number_from_int(0x0070), mk_symbol("xk-p"));
    hash_table_set(_key_table_, mk_number_from_int(0x0071), mk_symbol("xk-q"));
    hash_table_set(_key_table_, mk_number_from_int(0x0072), mk_symbol("xk-r"));
    hash_table_set(_key_table_, mk_number_from_int(0x0073), mk_symbol("xk-s"));
    hash_table_set(_key_table_, mk_number_from_int(0x0074), mk_symbol("xk-t"));
    hash_table_set(_key_table_, mk_number_from_int(0x0075), mk_symbol("xk-u"));
    hash_table_set(_key_table_, mk_number_from_int(0x0076), mk_symbol("xk-v"));
    hash_table_set(_key_table_, mk_number_from_int(0x0077), mk_symbol("xk-w"));
    hash_table_set(_key_table_, mk_number_from_int(0x0078), mk_symbol("xk-x"));
    hash_table_set(_key_table_, mk_number_from_int(0x0079), mk_symbol("xk-y"));
    hash_table_set(_key_table_, mk_number_from_int(0x007a), mk_symbol("xk-z"));
    hash_table_set(_key_table_, mk_number_from_int(0x007b), mk_symbol("xk-braceleft"));
    hash_table_set(_key_table_, mk_number_from_int(0x007c), mk_symbol("xk-bar"));
    hash_table_set(_key_table_, mk_number_from_int(0x007d), mk_symbol("xk-braceright"));
    hash_table_set(_key_table_, mk_number_from_int(0x007e), mk_symbol("xk-asciitilde"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a0), mk_symbol("xk-nobreakspace"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a1), mk_symbol("xk-exclamdown"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a2), mk_symbol("xk-cent"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a3), mk_symbol("xk-sterling"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a4), mk_symbol("xk-currency"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a5), mk_symbol("xk-yen"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a6), mk_symbol("xk-brokenbar"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a7), mk_symbol("xk-section"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a8), mk_symbol("xk-diaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00a9), mk_symbol("xk-copyright"));
    hash_table_set(_key_table_, mk_number_from_int(0x00aa), mk_symbol("xk-ordfeminine"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ab), mk_symbol("xk-guillemotleft"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ac), mk_symbol("xk-notsign"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ad), mk_symbol("xk-hyphen"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ae), mk_symbol("xk-registered"));
    hash_table_set(_key_table_, mk_number_from_int(0x00af), mk_symbol("xk-macron"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b0), mk_symbol("xk-degree"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b1), mk_symbol("xk-plusminus"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b2), mk_symbol("xk-twosuperior"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b3), mk_symbol("xk-threesuperior"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b4), mk_symbol("xk-acute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b5), mk_symbol("xk-mu"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b6), mk_symbol("xk-paragraph"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b7), mk_symbol("xk-periodcentered"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b8), mk_symbol("xk-cedilla"));
    hash_table_set(_key_table_, mk_number_from_int(0x00b9), mk_symbol("xk-onesuperior"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ba), mk_symbol("xk-masculine"));
    hash_table_set(_key_table_, mk_number_from_int(0x00bb), mk_symbol("xk-guillemotright"));
    hash_table_set(_key_table_, mk_number_from_int(0x00bc), mk_symbol("xk-onequarter"));
    hash_table_set(_key_table_, mk_number_from_int(0x00bd), mk_symbol("xk-onehalf"));
    hash_table_set(_key_table_, mk_number_from_int(0x00be), mk_symbol("xk-threequarters"));
    hash_table_set(_key_table_, mk_number_from_int(0x00bf), mk_symbol("xk-questiondown"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c0), mk_symbol("xk-Agrave"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c1), mk_symbol("xk-Aacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c2), mk_symbol("xk-Acircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c3), mk_symbol("xk-Atilde"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c4), mk_symbol("xk-Adiaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c5), mk_symbol("xk-Aring"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c6), mk_symbol("xk-AE"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c7), mk_symbol("xk-Ccedilla"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c8), mk_symbol("xk-Egrave"));
    hash_table_set(_key_table_, mk_number_from_int(0x00c9), mk_symbol("xk-Eacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ca), mk_symbol("xk-Ecircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00cb), mk_symbol("xk-Ediaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00cc), mk_symbol("xk-Igrave"));
    hash_table_set(_key_table_, mk_number_from_int(0x00cd), mk_symbol("xk-Iacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ce), mk_symbol("xk-Icircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00cf), mk_symbol("xk-Idiaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d0), mk_symbol("xk-ETH"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d0), mk_symbol("xk-Eth"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d1), mk_symbol("xk-Ntilde"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d2), mk_symbol("xk-Ograve"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d3), mk_symbol("xk-Oacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d4), mk_symbol("xk-Ocircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d5), mk_symbol("xk-Otilde"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d6), mk_symbol("xk-Odiaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d7), mk_symbol("xk-multiply"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d8), mk_symbol("xk-Oslash"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d8), mk_symbol("xk-Ooblique"));
    hash_table_set(_key_table_, mk_number_from_int(0x00d9), mk_symbol("xk-Ugrave"));
    hash_table_set(_key_table_, mk_number_from_int(0x00da), mk_symbol("xk-Uacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00db), mk_symbol("xk-Ucircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00dc), mk_symbol("xk-Udiaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00dd), mk_symbol("xk-Yacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00de), mk_symbol("xk-THORN"));
    hash_table_set(_key_table_, mk_number_from_int(0x00de), mk_symbol("xk-Thorn"));
    hash_table_set(_key_table_, mk_number_from_int(0x00df), mk_symbol("xk-ssharp"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e0), mk_symbol("xk-agrave"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e1), mk_symbol("xk-aacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e2), mk_symbol("xk-acircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e3), mk_symbol("xk-atilde"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e4), mk_symbol("xk-adiaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e5), mk_symbol("xk-aring"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e6), mk_symbol("xk-ae"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e7), mk_symbol("xk-ccedilla"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e8), mk_symbol("xk-egrave"));
    hash_table_set(_key_table_, mk_number_from_int(0x00e9), mk_symbol("xk-eacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ea), mk_symbol("xk-ecircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00eb), mk_symbol("xk-ediaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ec), mk_symbol("xk-igrave"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ed), mk_symbol("xk-iacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ee), mk_symbol("xk-icircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ef), mk_symbol("xk-idiaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f0), mk_symbol("xk-eth"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f1), mk_symbol("xk-ntilde"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f2), mk_symbol("xk-ograve"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f3), mk_symbol("xk-oacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f4), mk_symbol("xk-ocircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f5), mk_symbol("xk-otilde"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f6), mk_symbol("xk-odiaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f7), mk_symbol("xk-division"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f8), mk_symbol("xk-oslash"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f8), mk_symbol("xk-ooblique"));
    hash_table_set(_key_table_, mk_number_from_int(0x00f9), mk_symbol("xk-ugrave"));
    hash_table_set(_key_table_, mk_number_from_int(0x00fa), mk_symbol("xk-uacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00fb), mk_symbol("xk-ucircumflex"));
    hash_table_set(_key_table_, mk_number_from_int(0x00fc), mk_symbol("xk-udiaeresis"));
    hash_table_set(_key_table_, mk_number_from_int(0x00fd), mk_symbol("xk-yacute"));
    hash_table_set(_key_table_, mk_number_from_int(0x00fe), mk_symbol("xk-thorn"));
    hash_table_set(_key_table_, mk_number_from_int(0x00ff), mk_symbol("xk-ydiaeresis"));
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
    assert_throw(is_symbol(exp), 
                 TYPE_ERROR,
                 "GR_SET_FOREGROUND: expects a symbol");
    unsigned long colour;
    TYPE* c = hash_table_ref(_colour_table_, exp);

    assert_throw(!IS_NIL(c),
                 TYPE_ERROR,
                 "GR_SET_FOREGROUND: could not find colour");
    
    XSetForeground(display, 
                   gc, 
                   c->d.i);
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

        TYPE* key_symbol = hash_table_ref(_key_table_, mk_number_from_int(ks));
        
        if (IS_NIL(key_symbol))
        {
            key_symbol = mk_symbol("xk-unkown");
        }

        result = cons(mk_symbol("key-press"),
                      cons(window,
                           cons(mk_number_from_int(event.xkey.x),
                                cons(mk_number_from_int(event.xkey.y),
                                     cons(mk_number_from_int(event.xkey.state),
                                          cons(key_symbol,
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
