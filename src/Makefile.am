AM_YFLAGS = -t -v -d -Wno-yacc
BUILT_SOURCES = parser.h parser.c
CLEANFILES = parser.h parser.c parser.output lexer.c
bin_PROGRAMS = scheme
scheme_SOURCES = parser.y lexer.l number.c symbol.c symbol_table.c char.c str.c vector.c \
    procedure.c type.c port.c util.c io.c eval.c env.c error.c primitive_procedure.c \
	hash_table.c stack.c socket.c graphics.c blob.c unix.c read.c  elab.c syntax.c \
	prelude.c scheme.c
scheme_LDADD = -L/usr/local/lib -lX11 -L/usr/lib  -lm -ldl -lgc -lpthread
clean-local:
	-rm *~
