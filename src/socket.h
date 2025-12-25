// MIT license
//
// Copyright 2025 Per Nilsson
///
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
