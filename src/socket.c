#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <unistd.h>

#include <gc.h>

#include "symbol.h"
#include "error.h"
#include "number.h"
#include "socket.h"
#include "util.h"
#include "str.h"

/* arbitrary backlog value */
#define BACK_LOGG 50

static void
populate_sockaddr(const TYPE* addr, const TYPE* port, struct sockaddr_in* saddr)
{
    saddr->sin_family = AF_INET;
    assert_throw(port->d.i < 65535, TYPE_ERROR, "Port is to large");

    printf("populating ip port %d\n", port->d.i);

    saddr->sin_port = htons(port->d.i);

    if (strcmp(addr->d.s, "INADDR-ANY") == 0)
    {
        saddr->sin_addr.s_addr = INADDR_ANY;
    }
    else
    {
        printf("populating ip addr %s\n", addr->d.s);
        struct in_addr a;
        assert_throw(inet_aton(addr->d.s, &a) != 0, 
                     TYPE_ERROR, 
                     "Could not convert string ip to in_addr\n");
        
        saddr->sin_addr.s_addr = a.s_addr;
    }
}

TYPE* 
mk_udp_socket(const TYPE* addr, const TYPE* port)
{
    struct sockaddr_in saddr;

    assert_throw(is_string(addr) && is_number(port), 
                 TYPE_ERROR,
                 "MK_UDP_SOCKET: expexts a string and a number as arguments");
    
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_UDP_SOCKET: could not allocate memory for type");
        exit(1);
    }

    result->type = UDP_SOCKET;
    result->d.i = socket(PF_INET, SOCK_DGRAM, 0);
        
    if (result->d.i < 0)
    {
        perror("MK_UDO_SOCKET, socket");
        throw_error(OS_ERROR, "UDP_SOCKET: failed to create socket");
    }

    populate_sockaddr(addr, port, &saddr);
   
    if (bind(result->d.i, (struct sockaddr *) &saddr, sizeof(saddr)) < 0)
    {
        perror("MK_SOCKET, bind");
        throw_error(OS_ERROR, "UDP_SOCKET: failed to bind");
    }
    
    return result;
}


TYPE*
udp_socket_recv(const TYPE* socket, TYPE* buffer)
{
    assert_throw(socket->type == UDP_SOCKET && is_string(buffer), 
                 TYPE_ERROR,
                 "UDP_SOCKET_RECV: expects a socket and a string as arguments");
 
    int n_bytes = recv(socket->d.i, 
                       buffer->d.s, 
                       strlen(buffer->d.s), 
                       0);

    return mk_number_from_int(n_bytes);
}

void
udp_socket_sendto(const TYPE* socket, 
                  const TYPE* buffer, 
                  const TYPE* addr, 
                  const TYPE* port)
{
    struct sockaddr_in saddr;
    socklen_t size = sizeof(saddr);

    assert_throw(socket->type == UDP_SOCKET && 
                 is_string(buffer) && 
                 is_string(addr) && 
                 is_number(port), 
                 TYPE_ERROR,
                 "UDP_SOCKET_SENDTO: expects a socket, string, string "
                 "and a number as arguments");

    populate_sockaddr(addr, port, &saddr);
    
    int result = sendto(socket->d.i, 
                        buffer->d.s, 
                        strlen(buffer->d.s), 
                        0, 
                        (struct sockaddr *) &saddr, 
                        size);
 
    printf("sent %d bytes\n", result);

    if (result < 0)
    {
        perror("UDP_SOCKET_SEND");
    }
}

TYPE* 
mk_server_socket(const TYPE* addr, const TYPE* port)
{
    struct sockaddr_in saddr;

    assert_throw(is_string(addr) && is_number(port), 
                 TYPE_ERROR,
                 "MAKE_SERVER_SOCKET: expects a string and "
                 "a number as arguments");

    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_TCP_SOCKET: could not allocate memory for type");
        exit(1);
    }

    result->type = SERVER_SOCKET;
    result->d.i = socket(PF_INET, SOCK_STREAM, 0);
    
    if (result->d.i < 0)
    {
        perror("MAKE_SERVER_SOCKET");
        throw_error(OS_ERROR,
                    "MAKE_SERVER_SOCKET: could not create socker");
    }
        

    populate_sockaddr(addr, port, &saddr);

    if (bind ((int)result->d.i, (struct sockaddr *) &saddr, sizeof(saddr)) < 0)
    {
        perror("MAKE_SERVER_SOCKET");
        throw_error(OS_ERROR,
                    "MAKE_SERVER_SOCKET: could not bind socket");
    }
    
    if (listen((int)result->d.i, BACK_LOGG) < 0)
    {
        perror("MAKE_SERVER_SOCKET");
        throw_error(OS_ERROR,
                    "MAKE_SERVER_SOCKET: could not listen on socket");
    }

    return result;
}

TYPE* 
server_socket_accept(const TYPE* server_socket)
{
    assert_throw(server_socket->type == SERVER_SOCKET, 
                 TYPE_ERROR,
                 "SERVER_SOCKET_ACCEPT: expects a socket as argument");

    TYPE* result;
    struct sockaddr_in clientname;
    int size = sizeof(clientname);
    int fd = accept(server_socket->d.i,
                    (struct sockaddr *) &clientname,
                    &size);
    if (fd < 0)
    {
        perror ("accept");
        exit (EXIT_FAILURE);
    }

    fprintf(stderr,
            "Server: connect from host %s, port %u.\n",
            inet_ntoa (clientname.sin_addr),
            ntohs (clientname.sin_port));
    
    result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_TCP_SOCKET: could not allocate memory for type");
        exit(1);
    }

    result->type = TCP_SOCKET;
    result->d.i = fd;
    
    return result;
}

TYPE* 
mk_tcp_socket(const TYPE* addr, const TYPE* port)
{
    struct sockaddr_in saddr;

    assert_throw(is_string(addr) && is_number(port), 
                 TYPE_ERROR,
                 "MK_TCP_SOCKET: expexts a string and a number as arguments");
    
    TYPE* result = mloc(sizeof(TYPE));
    
    if (result == NULL)
    {
        fprintf(stderr, "MK_TCP_SOCKET: could not allocate memory for type");
        exit(1);
    }

    result->type = TCP_SOCKET;
    result->d.i = socket(PF_INET, SOCK_STREAM, 0);
        
    if (result->d.i < 0)
    {
        perror("MK_TCP_SOCKET, socket");
        throw_error(OS_ERROR, "MK_TCP_SOCKET: failed to make socket");
    }

    populate_sockaddr(addr, port, &saddr);
   
    if (connect(result->d.i, 
                (struct sockaddr *) &saddr, 
                sizeof(saddr)) < 0)
    {
        perror("MK_TCP_SOCKET, connect");
        throw_error(OS_ERROR, "MK_TCP_SOCKET: failed to connect");
    }
    
    return result;
}

TYPE* 
tcp_socket_recv(const TYPE* socket, TYPE* buffer)
{
    int n_bytes;

    assert_throw(socket->type == TCP_SOCKET && is_string(buffer), 
                 TYPE_ERROR,
                 "TCP_SOCKET_RECV: expects a socket and a string as arguments");
 
    n_bytes = recv(socket->d.i, 
                   buffer->d.s, 
                   strlen(buffer->d.s), 
                   0);

    return mk_number_from_int(n_bytes);

}

void 
tcp_socket_send(const TYPE* tcp_socket, const TYPE* buffer)
{
    int n_bytes;
    
    assert_throw(tcp_socket->type == TCP_SOCKET && is_string(buffer), 
                 TYPE_ERROR,
                 "TCP_SOCKET_SEND: expects a socket and a string as arguments");

    n_bytes = write(tcp_socket->d.i, 
                    buffer->d.s, 
                    strlen(buffer->d.s));

    if (n_bytes < 0)
    {
        perror("tcp_socket_send");
        throw_error(OS_ERROR, "TCP_SOCKET_SEND: failed to write to fd");
    }
}

void 
socket_close(const TYPE* socket)
{
    assert_throw(socket->type == UDP_SOCKET ||
                 socket->type == TCP_SOCKET ||
                 socket->type == SERVER_SOCKET, 
                 TYPE_ERROR,
                 "SOCKET_CLOSE: expects a socket as argument");

    close(socket->d.i);
}
