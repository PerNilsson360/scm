#!/home/per/git/scm/scheme
;real	0m9,007s
;user	0m10,845s
;sys	0m0,445s
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))
(display (fib 30))
