CC = gcc
CFLAGS = -g -Winline -Wl,-defsym,_DYNAMIC=0 # -pedantic -Werror -v 

UT_OBJ = env_ut.o symbol_ut.o
OBJECTS = \
	number.o symbol.o char.o str.o vector.o procedure.o type.o \
	port.o util.o io.o eval.o env.o error.o primitive_procedure.o \
	hash_table.o stack.o socket.o graphics.o blob.o unix.o read.o \
	elab.o	

LIBS = -L/usr/local/lib -lX11 -L/usr/lib  -lm -ldl -lgc -lpthread
INC = -I/usr/local/include

main : $(OBJECTS) $(UT_OBJ)
	$(CC) $(CFLAGS) $(OBJECTS) scheme.c -o scheme $(LIBS)
	$(CC) $(CFLAGS) $(OBJECTS) $(UT_OBJ) ut.c -o ut $(LIBS)
#	ut

%.o : %.c %.h Makefilen
	$(CC) $(CFLAGS) -c $<
test : test.c
	$(CC) $(CFLAGS) test.c -o test -lX11
clean :
	rm *.o
	rm scheme
	rm TAGS
	rm tst/*~
	rm *~

tags :
	find . -name "*.[chCH]" -print | etags -	

check-syntax :	
	$(CC) $(CFLAGS) $(CHK_SOURCES) -c $(CHK_SOURCES)
