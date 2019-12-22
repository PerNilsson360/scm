#!/home/per/git/scm/scheme

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))

(let ((num (caddr argv)))
  (display (fib (string->number num)))
)
