#ifndef _SOCKET_H_
#define _SOCKET_H_
#include "type.h"

TYPE* mk_udp_socket(const TYPE* addr, const TYPE* port);
TYPE* udp_socket_recv(const TYPE* udp_socket, TYPE* buffer);
void udp_socket_sendto(const TYPE* udp_socket, 
                       const TYPE* buffer, 
                       const TYPE* addr, 
                       const TYPE* port); 

TYPE* mk_server_socket(const TYPE* addr, const TYPE* port);
TYPE* server_socket_accept(const TYPE* server_socket);

TYPE* mk_tcp_socket(const TYPE* addr, const TYPE* port);
TYPE* tcp_socket_recv(const TYPE* tcp_socket, TYPE* buffer);
void tcp_socket_send(const TYPE* tcp_socket, const TYPE* buffer);

void socket_close(const TYPE* socket);

#endif
