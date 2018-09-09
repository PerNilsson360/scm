CC = gcc
CFLAGS = -g -Winline -Wl,-defsym,_DYNAMIC=0 # -pedantic -Werror -v 

UT_OBJ = env_ut.o symbol_ut.o
OBJECTS1 = number.o symbol.o char.o str.o vector.o procedure.o type.o 
OBJECTS2 = port.o util.o io.o eval.o env.o error.o primitive_procedure.o 
OBJECTS3 = hash_table.o stack.o socket.o graphics.o blob.o unix.o read.o
OBJECTS = $(OBJECTS1) $(OBJECTS2) $(OBJECTS3)
LIBS = -L/usr/local/lib -lX11 -L/usr/lib  -lm -ldl -lgc -lpthread
INC = -I/usr/local/include

main : $(OBJECTS) $(UT_OBJ)
	$(CC) $(CFLAGS) $(OBJECTS) scheme.c -o scheme $(LIBS)
#	$(CC) $(CFLAGS) $(OBJECTS) $(UT_OBJ) ut.c -o ut
#	ut

%.o : %.c %.h makefilen
	$(CC) $(CFLAGS) -c $<
test : test.c
	$(CC) $(CFLAGS) test.c -o test -lX11
clean :
	rm scheme
	rm TAGS
	rm *.o
	rm tst/*~
	rm *~

tags :
	find . -name "*.[chCH]" -print | etags -	

check-syntax :	
	$(CC) $(CFLAGS) $(CHK_SOURCES) -c $(CHK_SOURCES)
