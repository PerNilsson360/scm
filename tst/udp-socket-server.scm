(define (server)
  (define (inner socket buffer)
    (let* ((n (udp-socket-recv! socket buffer))
	   (s (substring buffer 0 n)))
      (begin
	(display "received: ") 
	(newline) 
	(display n) 
	(newline) 
	(display s)
	(newline)
	(inner socket buffer))))
    (let ((socket (make-udp-socket "192.168.56.1" 9999))
	(buffer (make-string 100)))
    (inner socket buffer)))

(define (client)
  (let ((socket (make-udp-socket "130.100.96.92" 8888)))
    (udp-socket-sendto socket "hello world" "130.100.96.92" 9999)))

(server)