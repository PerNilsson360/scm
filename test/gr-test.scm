#!/home/per/git/scm/scheme

(load "/home/per/git/scm/srfi9-records.scm")
(load "/home/per/prg/c++/scheme/keysymdef.scm")
(load "/home/per/git/scm/rgb-colors.scm")
(load "/home/per/git/scm/tst/2d-geometry.scm")

(define speed 0.2) 						; 10 pixels a second (50 hz) 1/5 = 0.2 

(define (print-line s) (display s) (newline))
(define (dec n) (- n 1))

(define (make-mage window v)
  (define (draw x y) 
    (print-line (string-append "drawing at x: " (number->string x)
							   " y: " (number->string x)))
    (gr-set-foreground blue)
    (gr-move-to! x y) 
    (x-fill-arc window 20 20 0 (* 360 64))
    (x-flush))
  (let ((destination (cons x y)))
    (lambda (message)
      (match message
	    ((move ?x ?y) (begin (set! destination (cons x y))
							 (print-line destination)))
	    (animate (if (not (equal? destination (cons x y)))
					 (begin (draw x y) 
							(set! x (car destination)) 
							(set! y (cdr destination))
							(draw x y))))
	    (draw (draw x y))
	    (? (print-line (string-append "MAGE: not a supported event: " 
									  (symbol->string message))))))))

(define (schedule)
  (let ((running #t)
		(mage (make-mage (gr-root-win) 30 30)))
    (define (handle-events n-events)
      (if (> n-events 0)
		  (let ((event (x-next-event)))
			(match event
			  ((expose . ?) 
		       (begin (print-line event)
					  (mage 'draw) 
					  (handle-events (dec n-events))))
			  ((button-press ?win ?x ?y . ?) 
		       (begin (mage `(move ,x ,y)) 
					  (handle-events (dec n-events))))
			  ((key-press ?win ?x ?y ?state ?key ?str) 
		       (cond ((= key xk-Q) 
					  (begin (print-line event) (set! running #f)))
					 (else (begin (print-line event) 
								  (handle-events (dec n-events))))))
			  (? (begin (print-line event) 
						(handle-events (dec n-events))))))))
    (define (animate)
      (mage 'animate))
    (let loop ()
      (if running
		  (let ((n-events (x-events-queued)))
			(if (= n-events 0)
	  			(begin (animate) (nano-sleep 0 20000000) (loop)) ;0,02s 50 hz
	  			(begin (handle-events n-events) (loop))))))))

(define (run)
  (gr-open)
  (schedule)
  (display "closing  X")
  (newline)
  (gr-close))

(run)
