#include <stdlib.h>
#include <stdio.h>

#include <gc.h>

#include "common.h"
#include "error.h"
#include "port.h"
#include "str.h"

static
type* 
mk_port(FILE* file, int read_port)
{
    type* result = mloc(sizeof(type));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_PORT: could not allocate memory for type");
        exit(1);
    }

    result->data = mloc(sizeof(PORT_DATA));

    if (result->data == NULL)
    {
        fprintf(stderr, "MK_PORT: could not allocate memory for PORT_DATA");
        exit(1);
    }

    result->type = PORT;

    ((PORT_DATA*) result->data)->read_port = read_port;
    ((PORT_DATA*) result->data)->file = file;

    return result;
}

type* 
is_port(const type* obj)
{
    return mk_boolean(obj->type == PORT);
}

type* 
is_input_port(const type* obj)
{
    return mk_boolean(obj->type == PORT && ((PORT_DATA*) obj->data)->read_port);
}

type* 
is_output_port(const type* obj)
{
    return mk_boolean(is_port(obj) && !((PORT_DATA*) obj->data)->read_port);
}

type* 
open_input_file(const type* filename)
{
    FILE* file;
    
    assert_throw(is_string(filename), 
                 TYPE_ERROR, 
                 "OPEN_INPUT_FILE: filename is not a string");

     file = fopen((char*) filename->data, "r");

     if (file == NULL)
     {
         throw_error(OS_ERROR, 
                     "OPEN_INPUT_FILE: could not open file");
     }

     return mk_port(file, TRUE);
}

type* 
open_output_file(const type* filename)
{
    FILE* file;
    
    assert_throw(is_string(filename), 
                 TYPE_ERROR, 
                 "OPEN_INPUT_FILE: filename is not a string");

     file = fopen((char*) filename->data, "w");

     if (file == NULL)
     {
         throw_error(OS_ERROR, 
                     "OPEN_OUTPUT_FILE: could not open file");
     }

     return mk_port(file, FALSE);
}

void 
close_input_port(const type* port)
{
    assert_throw(is_true(is_port(port)), 
                 TYPE_ERROR, 
                 "CLOSE_INPUT_PORT: port is not a port");
    
    if (fclose((FILE*) port->data) == EOF)
    {
        printf("CLOSE_INPUT_PORT: error detected when closing input port");
    }
}

void 
close_output_port(const type* port)
{
    assert_throw(is_true(is_port(port)), 
                 TYPE_ERROR, 
                 "CLOSE_OUTPUT_PORT: port is not a port");
    
    if (fclose((FILE*) port->data) == EOF)
    {
        printf("CLOSE_OUTPUT_PORT: error detected when closing output port");
    }
}

