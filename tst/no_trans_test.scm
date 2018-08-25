(define expect
  (lambda (x) 
    (if (not x)
	(error "Unit test failure"))))

(define length
  (lambda (li)
    (if (null? li)
	0
	(+ 1 (length (cdr li))))))

(define reverse 
  (lambda (list)
    (define inner 
      (lambda (in out)
	(if (null? in) 
	    out
	    (inner (cdr in) (cons (car in) out)))))
    (inner list '())))

(define memv
  (lambda (a li)
    (cond ((null? li) #f)
	  ((eqv? a (car li)) li)
	  (else (memv a (cdr li))))))

(define list-equal?
  (lambda (a b)
    (if (null? a)
	(if (null? b)
	    #t
	    #f)
	(if (eq? (car a) (car b))
	    (list-equal? (cdr a) (cdr b))
	    #f))))

(define li '(0 1 2 3 4 5 6 7 8 9))
(define li1 '(0 1 2 3 ))
(define li2 '(4 5 6 7 8 9))

(define test-list
  (lambda ()
    (expect (= (length li 10)))
    (expect (list-equal? (reverse (reverse li)) li))
    (expect (list-equal? (memv 4 li) li2))
    ))

(define divisible-by-two?
  (lambda (n)
    (define even?
      (lambda (n)
	(if (= n 0)
	    #t
	    (odd? (- n 1)))))
    (define odd?
      (lambda (n)
	(if (= n 0)
	    #f
	    (even? (- n 1)))))
    (even? n)))

(define test-internal-defs
  (lambda ()
    (expect (divisible-by-two? 1000))
    (expect (not (divisible-by-two? 10001)))
    ))

(define accumulate
  (lambda (f identity list)
    (if (null? list) 
	identity
	(f (car list) (accumulate f identity (cdr list))))))

(define test-higher-order
  (lambda ()
    (expect (= (accumulate + 0 '(1 1 1 1 1 1)) 6))
    ))
    
(define test
  (lambda ()
    (test-list)
    (test-internal-defs)
    (test-higher-order)
    (display "tests ok")
    ))

(test)

; (load "no_trans_test.scm")
