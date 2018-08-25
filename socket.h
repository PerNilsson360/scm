#ifndef _SOCKET_H_
#define _SOCKET_H_
#include "type.h"

type* mk_udp_socket(const type* addr, const type* port);
type* udp_socket_recv(const type* udp_socket, type* buffer);
void udp_socket_sendto(const type* udp_socket, 
                       const type* buffer, 
                       const type* addr, 
                       const type* port); 

type* mk_server_socket(const type* addr, const type* port);
type* server_socket_accept(const type* server_socket);

type* mk_tcp_socket(const type* addr, const type* port);
type* tcp_socket_recv(const type* tcp_socket, type* buffer);
void tcp_socket_send(const type* tcp_socket, const type* buffer);

void socket_close(const type* socket);

#endif
