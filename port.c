#include <stdlib.h>
#include <stdio.h>

#include <gc.h>

#include "common.h"
#include "error.h"
#include "port.h"
#include "str.h"
#include "util.h"

static
TYPE* 
mk_port(FILE* file, int read_port)
{
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_PORT: could not allocate memory for type");
        exit(1);
    }

    result->d.po = mloc(sizeof(PORT_DATA));

    if (result->d.po == NULL)
    {
        fprintf(stderr, "MK_PORT: could not allocate memory for PORT_DATA");
        exit(1);
    }

    result->type = PORT;

    result->d.po->read_port = read_port;
    result->d.po->file = file;

    return result;
}

TYPE* 
is_port(const TYPE* obj)
{
    return mk_boolean(obj->type == PORT);
}

TYPE* 
is_input_port(const TYPE* obj)
{
    return mk_boolean(obj->type == PORT && obj->d.po->read_port);
}

TYPE* 
is_output_port(const TYPE* obj)
{
    return mk_boolean(is_port(obj) && !obj->d.po->read_port);
}

TYPE* 
open_input_file(const TYPE* filename)
{
    FILE* file;
    
    assert_throw(is_string(filename), 
                 TYPE_ERROR, 
                 "OPEN_INPUT_FILE: filename is not a string");

     file = fopen((char*) filename->d.s, "r");

     if (file == NULL)
     {
		 fprintf(stderr, "filename: %s\n", filename->d.s);
         throw_error(OS_ERROR, 
                     "OPEN_INPUT_FILE: could not open file");
     }

     return mk_port(file, TRUE);
}

TYPE* 
open_output_file(const TYPE* filename)
{
    FILE* file;
    
    assert_throw(is_string(filename), 
                 TYPE_ERROR, 
                 "OPEN_INPUT_FILE: filename is not a string");

     file = fopen(filename->d.s, "w");

     if (file == NULL)
     {
		 fprintf(stderr, "filename: %s\n", filename->d.s);
         throw_error(OS_ERROR, 
                     "OPEN_OUTPUT_FILE: could not open file");
     }

     return mk_port(file, FALSE);
}

void 
close_input_port(const TYPE* port)
{
    assert_throw(is_true(is_port(port)), 
                 TYPE_ERROR, 
                 "CLOSE_INPUT_PORT: port is not a port");
    
    if (fclose(port->d.po->file) == EOF)
    {
        printf("CLOSE_INPUT_PORT: error detected when closing input port");
    }
}

void 
close_output_port(const TYPE* port)
{
    assert_throw(is_true(is_port(port)), 
                 TYPE_ERROR, 
                 "CLOSE_OUTPUT_PORT: port is not a port");
    
    if (fclose(port->d.po->file) == EOF)
    {
        printf("CLOSE_OUTPUT_PORT: error detected when closing output port");
    }
}

