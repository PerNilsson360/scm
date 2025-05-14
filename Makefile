CC = gcc
CFLAGS = -O4 -Winline -Wl,-defsym,_DYNAMIC=0 # -pedantic -Werror -v 

OBJECTS = \
	number.o symbol.o char.o str.o vector.o procedure.o type.o \
	port.o util.o io.o eval.o env.o error.o primitive_procedure.o \
	hash_table.o stack.o socket.o graphics.o blob.o unix.o read.o \
	elab.o syntax.o lex.yy.o y.tab.o

LIBS = -L/usr/local/lib -lX11 -L/usr/lib  -lm -ldl -lgc -lpthread
INC = -I/usr/local/include

main : $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) scheme.c -o scheme $(LIBS)

%.o : %.c %.h Makefilen
	$(CC) $(CFLAGS) -c $<

y.tab.c : parser.y
	bison --verbose -y -d parser.y

lex.yy.c : lexer.l y.tab.o
	flex -d lexer.l #-d

test : test.c
	$(CC) $(CFLAGS) test.c -o test -lX11
clean :
	rm *.o
	rm scheme
	rm lex.yy.c
	rm y.output
	rm y.tab.c
	rm y.tab.h
	rm TAGS
	rm tst/*~
	rm *~

tags :
	find . -name "*.[chCH]" -print | etags -	

check-syntax :	
	$(CC) $(CFLAGS) $(CHK_SOURCES) -c $(CHK_SOURCES)
