(define (server)
  (define (receive-loop socket buffer)
    (let ((n (tcp-socket-recv! socket buffer)))
      (cond ((< n 0) (display "Receive-loop: got error"))
	    ((= n 0) (display "Receive-loop: got eof"))
	    (else (let ((s (substring buffer 0 n)))
		    (begin
		      (display "received: ") 
		      (newline) 
		      (display n) 
		      (newline) 
		      (display s)
		      (newline)
		      (receive-loop socket buffer)))))))
  (let* ((server-socket (make-server-socket "130.100.96.51" 9999))
	 (socket (server-socket-accept server-socket))
	 (buffer (make-string 1000)))
    (receive-loop socket buffer)
    (socket-close server-socket)
    (socket-close socket)))

(define (client)
  (let ((socket (make-tcp-socket "130.100.96.51" 9999)))
    (begin (tcp-socket-send 
	    socket 
	    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
               <hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
                 <capabilities>
	        <capability>urn:ietf:params:netconf:base:1.0</capability>
             </capabilities>
            </hello>]]>]]>")
	   (socket-close socket))))

(server)