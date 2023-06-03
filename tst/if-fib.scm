#!/home/per/git/scm/scheme
;real	0m16,547s					   
;user	0m18,766s
;sys	0m0,707s
(define (fib n)
  (if (= n 0)
	  0
	  (if (= n 1)
		  1
		  (+ (fib (- n 1)) (fib (- n 2))))))
(display (fib 30))
