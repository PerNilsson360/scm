#!/usr/local/bin/scheme
;real	0m4,513s
;user	0m6,328s
;sys	0m0,408s
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))
(display (fib 35))
