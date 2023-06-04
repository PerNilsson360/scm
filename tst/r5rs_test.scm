#!/home/per/git/scm/scheme

(define (expect x)
  (if (not (eval x environment))
      (error "Unit test failure" x)))

(define fact
  (lambda (n)
	(if (= n 0)
		1
		(* n (fact (- n 1))))))

(define reverse-subtract (lambda (x y) (- y x)))

(define add4
  (let ((x 4))
	(lambda (y) (+ x y))))

(define (test-simple-examples)
  (expect '(equal? (* 5 8) 40))
  (expect '(equal? (fact 5) 120))
  (expect '(equal? (+ 3 4) 7))
  (expect '(equal? ((if #f + *) 3 4) 12))
  (expect '(procedure? (lambda (x) (+ x x))))
  (expect '(equal? ((lambda (x) (+ x x )) 4) 8))
  (expect '(equal? (reverse-subtract 7 10) 3))
  (expect '(equal? (add4 6) 10))
  ; (expect '(equal? ((lambda x x) '(3 4 5 6)) '(3 4 5 6))) 
  (expect '(equal? ((lambda (x y . z) z) 3 4 5 6) '(5 6)))
  )

;; 4.1.5
(define (test-conditionals)
  (expect '(equal? (if (> 3 2) 'yes 'no) 'yes))
  (expect '(equal? (if (> 2 3) 'yes 'no) 'no))
  (expect '(equal? (if (> 3 2) (- 3 2) (+ 3 2)) 1))
  )

;; 4.1.6
(define x 2)
(define (test-assignemnt)
  (expect '(equal? (+ x 1) 3))
  (expect '(equal? (begin (set! x 4) (+ x 1)) 5))
  )
		  
;; 4.2.1
(define (test-cond)
  (expect '(equal? (cond ((> 3 2) 'greater)
			 ((< 3 2) 'less))
		   'greater))
  (expect '(equal? (cond ((> 3 3) 'greater)
			 ((< 3 3) 'less)
			 (else 'equal))
		   'equal))
  ;; (expect (equal? (cond ((assv 'b '((a 1) (b 2))) => cadr)
  ;; 			(else #f))
  ;; 		  2))
  ;; not in r5rs
  (expect '(equal? (let ((a 1)) (cond ((= a 1) (set! a 2) (set! a 3))) a) 3))
  )

;; 4.2.1
(define (test-case)
  (expect '(equal?
			(case (* 2 3)
			  ((2 3 5 7) 'prime)
			  ((1 4 6 8 9) 'composite))
			'composite))
  ;; (expect (case (car '(c d)) how to test unspecified
  ;; 	   ((a) 'a)
  ;; 	   ((b) 'b)) unspecifed)
  (expect '(equal?
			(case (car '(c d))
			  ((a e i o u) 'vowel)
			  ((w y) 'semivowel)
			  (else 'consonant))
			'consonant))
  )

;; 4.2.1
(define (test-and)
  (expect '(and (= 2 2) (> 2 1)))
  (expect '(not (and (= 2 2) (< 2 1))))
  (expect '(equal? (and 1 2 'c '(f g)) '(f g)))
  (expect '(and))
  (expect '(and #t (and #t)))
  )

;; 4.2.1
(define (test-or)
  (expect '(or (= 2 2) (> 2 1)))
  (expect '(or (= 2 2) (< 2 1)))
  (expect '(not (or #f #f #f)))
  (expect '(equal? (or (memq 'b '(a b c))
		       (/ 3 0)) 
		   '(b c)))
  (expect '(or #f (or #f #t)))			; not in spec
  )


;; 4.2.2
(define (test-let)
  (expect '(= (let ((x 2) (y 3))
		(* x y))
	      6))
  (expect '(= (let ((x 2) (y 3))
		(let ((x 7)
		      (z (+ x y)))
		  (* z x)))
	      35))
  (expect '(= (let ((x 2) (y 3))
		(let* ((x 7)
		       (z (+ x y)))
		  (* z x)))
	      70))
  (expect '(letrec ((even?
		     (lambda (n)
		       (if (zero? n)
			   #t
			   (odd? (- n 1)))))
		    (odd?
		     (lambda (n)
		       (if (zero? n)
			   #f
			   (even? (- n 1))))))
	     (even? 88)))
  (expect '(equal? (let loop ((numbers '(3 -2 1 6 -5))
			      (nonneg '())
			      (neg '()))
		     (cond ((null? numbers) (list nonneg neg))
			   ((>= (car numbers) 0)
			    (loop (cdr numbers)
				  (cons (car numbers) nonneg)
				  neg))
			   ((< (car numbers) 0)
			    (loop (cdr numbers)
				  nonneg
				  (cons (car numbers) neg)))))
		   '((6 1 3) (-5 -2))))
  (expect '(let ((a (or #f (or #t)))) a))
  (expect '(let () #t))
  )

;; 4.2.4
(define (test-iteration)
  (expect '(equal?
			(let loop ((numbers '(3 -2 1 6 -5))
					   (nonneg '())
					   (neg '()))
			  (cond ((null? numbers) (list nonneg neg))
					((>= (car numbers) 0)
					 (loop (cdr numbers)
						   (cons (car numbers) nonneg)
						   neg))
					((< (car numbers) 0)
					 (loop (cdr numbers)
						   nonneg
						   (cons (car numbers) neg)))))
			'((6 1 3)(-5 -2))))
  )

;; 4.2.6
(define (test-quasiquote)
  (expect '(equal? `(list ,(+ 1 2) 4) '(list 3 4)))
  (expect '(equal? (let ((name 'a)) `(list ,name ',name)) 
				   '(list a (quote a))))
  ;(expect '(equal? `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
  ;				   '(a 3 4 5 b)))
  ;(expect '(equal? `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
  ;				   '((foo 7) . cons)))
  )

(define (test-quote)
  (expect '(symbol? (quote a)))
  (expect '(equal? (quote #(a b c)) '#(a b c)))
  (expect '(equal? (quote (+ 1 2)) '(+ 1 2)))
  (expect '(equal? '#(a b c) #(a b c)))
  (expect '(equal? ''a '(quote a)))
  (expect '(not (symbol? '())))
  (expect '(null? '()))
  )

;; 5.2.2
(define (test-internal-defintions)
  (expect '(equal? (let ((x 5))
					 (define foo (lambda (y) (bar x y)))
					 (define bar (lambda (a b) (+ (* a b) a)))(test-iteration)
					 (foo (+ x 3)))
				   45))
  (expect '(equal? (let ((x 5))
					 (letrec ((foo (lambda (y) (bar x y)))
							  (bar (lambda (a b) (+ (* a b) a))))
					   (foo (+ x 3))))
				   45))
  )

;; 6.1

(define gen-counter
  (lambda ()
	(let ((n 0))
	  (lambda () (set! n (+ n 1)) n))))

(define gen-loser
  (lambda ()
	(let ((n 0))
	  (lambda () (set! n (+ n 1)) 27))))

(define (test-eqv?)
  (expect '(not (eqv? 1 "")))
  ;; tests from list
  (expect '(eqv? 'a 'a))
  (expect '(not (eqv? 'a 'b)))
  (expect '(eqv? 2 2))
  (expect '(eqv? '() '()))
  (expect '(not (eqv? (cons 1 2) (cons 1 2))))
  (expect '(not (eqv? (lambda () 1) (lambda () 2))))
  (expect '(not (eqv? #f '())))
  (expect '(let ((p (lambda (x) x))) (eqv? p p)))
  (expect '(let ((g (gen-counter))) (eqv? g g)))
  (expect '(not (eqv? (gen-counter) (gen-counter))))
  (expect '(let ((g (gen-loser))) (eqv? g g)))
  ;(expect '(letrec ((f (lambda () (if (eqv? f g ) 'f 'both)))
  ;                  (g (lambda () (if (eqv? f g ) 'g 'both))))
  ;			 (eqv? f g)))
  (expect '(let ((x '(a))) (eqv? x x )))
  )

(define (test-eq?)
  (expect '(eq? 'a 'a))
  (expect '(not (eq? (list 'a) (list 'a))))
  (expect '(eq? '() '()))
  (expect '(eq? car car))
  (expect '(let ((x '(a))) (eq? x x)))
  (expect '(let (( x '#())) (eq? x x)))
  (expect '(let ((p (lambda (x) x))) (eq? p p)))
  )

(define (test-equal?)
  (expect '(equal? 'a 'a))
  (expect '(equal? '(a) '(a)))
  (expect '(equal? '(a (b) c) '(a (b) c)))
  (expect '(equal? "abc" "abc"))
  (expect '(equal? 2 2))
  (expect '(equal? (make-vector 5 'a) (make-vector 5 'a)))
  )

;; 6.2.4 
;; todo

;; 6.3.1
(define (test-booleans)
  (expect '(equal? (not #t) #f))
  (expect '(equal? (not 3) #f))
  (expect '(equal? (not (list 3)) #f))
  (expect '(equal? (not #f) #t))
  (expect '(equal? (not '()) #f))
  (expect '(equal? (not (list)) #f))
  (expect '(equal? (not 'nil) #f))
  (expect '(equal? (boolean? #f) #t))
  (expect '(equal? (boolean? 0) #f))
  (expect '(equal? (boolean? '()) #f))
  )

;; 6.3.2
(define x-6-3-2 (list 'a 'b 'c))
(define y x-6-3-2)
(define (test-pairs-and-lists)
  (expect '(equal? y '(a b c)))
  (expect '(equal? (begin (set-cdr! x-6-3-2 4) x-6-3-2) '(a . 4)))
  (expect '(equal? (list? y) #f))
  ; todo loopin (expect '(equal? (begin (set-cdr! x-6-3-2 x-6-3-2)) (list? x-6-3-2)))
  (expect '(pair? '(a . b)))
  (expect '(pair? '(a b c)))
  (expect '(not (pair? '())))
  (expect '(not (pair? '#(a b))))
  (expect '(equal? (cons 'a '()) '(a)))
  (expect '(equal? (cons '(a) '(b c d)) '((a) b c d )))
  (expect '(equal? (cons 'a 3) '(a .  3)))
  (expect '(equal? (cons '(a b) 'c) '((a b) . c)))
  (expect '(equal? (cdr '((a) b c d)) '(b c d)))
  (expect '(equal? (cdr '(1 . 2)) 2))
  (expect '(list? '(a b c)))
  (expect '(list? '()))
  (expect '(not (list? '(a .b))))
  ; todo crash (expect '(not (let ((x (list 'a))) (set-cdr! x x) (list? x))))
  (expect '(equal? (list 'a (+ 3 4) 'c) '(a 7 c)))
  (expect '(equal? (length '(a b c)) 3))
  (expect '(equal? (length '(a (b) (c d e))) 3))
  (expect '(equal? (append '(x) '(y)) '(x y)))
  (expect '(equal? (append '(a) '(b c d)) '(a b c d)))
  (expect '(equal? (append '(a (b)) '((c))) '(a (b) (c))))
  (expect '(equal? (reverse '(a b c)) '(c b a)))
  (expect '(equal? (reverse '(a (b c) d (e (f)))) '((e (f)) d (b c) a)))
  (expect '(equal? (list-ref '(a b c d) 2) 'c))
  (expect '(equal? (list-ref '(a b c d) (round 1.8)) 'c)) ; (inexact->exact (round 1.8))
  )

;;
(define (test-symbols)
  (expect '(symbol? 'foo))
  (expect '(symbol? (car '(a b))))
  (expect '(not (symbol? "bar")))
  (expect '(symbol? 'nil))
  (expect '(not (symbol? '())))
  (expect '(not (symbol? #f)))
  (expect '(equal? (symbol->string 'flying-fish) "flying-fish"))
  ; todo use default lowercase (expect '(equal? (symbol->string 'marting) "martin"))
  (expect '(equal? (symbol->string (string->symbol "Malvina")) "Malvina"))
  ; todo does not even parse (expect '(eq? 'mISSISSIppi 'mississippi))
  )

;; 6.3.4 Characters
(define (test-characters)
  ; todo (expect '(char=? #\A #\B))
  ; (expect '(char<? #\a #\b))
  ;(expect '(char<? #\0 #\9))
  ;(expect '(char-ci=? #\A #\a))
  ;; todo need more testcase
  (display "todo test characters") (newline)
  )

;; 6.3.5 Strings

;; 6.3.6 Vectors
(define (test-vectors)
  (expect '(equal? (vector 'a 'b 'c) #(a b c)))
  (expect '(equal? (vector-ref '#(1 1 2 3 5 8 13 21) 5) 8))
  )

;; 6.4

(define compose
  (lambda (f g)
	(lambda args
	  (f (apply g args)))))

(define a-stream (letrec ((next (lambda (n) (cons n (delay (next (+ n 1)))))))
						  (next 0)))
(define head car)
(define tail (lambda (stream) (force (cdr stream))))

(define (test-control-features)
  (expect '(procedure? car))
  (expect '(not(procedure? 'car)))
  (expect '(procedure? (lambda (x) (* x x))))
  (expect '(not(procedure? '(lambda (x) (* x x)))))
  (expect '(equal? (apply + (list 3 4)) 7))
  (expect '(equal? (apply + 1 (list 1 1)) 3))	 ; not in r5rs 
  (expect '(equal? (apply (lambda () 1) '()) 1)) ; 0 arguments
  ; TODO lambda no paren on arg bug (expect '(equal? ((compose sqrt *) 12 75) 30))
  (expect '(equal? (map cadr '((a b) (d e) (g h))) '(b e h)))
  (expect '(equal? (map (lambda (n) (expt n n)) '(1 2 3 4 5)) '(1 4 27 256 3125)))
  (expect '(equal? (map + '(1 2 3) '(4 5 6)) '(5 7 9)))
  (expect '(equal? (let ((v (make-vector 5)))
					 (for-each (lambda (i) (vector-set! v i (* i i)))
							   '(0 1 2 3 4))
					 v)
				   #(0 1 4 9 16)))
  (expect '(equal? (force (delay (+ 1 2))) 3))
  (expect '(equal? (let ((p (delay (+ 1 2)))) (list (force p) (force p))) '(3 3)))
  (expect '(equal? (head (tail (tail a-stream))) 2))
  )

(define (test-memq?)
  (expect '(equal? (memq 'a '(a b c)) '(a b c)))
  (expect '(equal? (memq 'b '(a b c)) '(b c)))
  (expect '(equal? (memq 'a '(b c d)) #f))
  (expect '(equal? (memq (list 'a) '(b (a) c)) #f))
  (expect '(equal? (member (list 'a) '(b (a) c)) '((a) c)))
					; (expect (equal? (memq 101 ’(100 101 102)) unspecified
  (expect '(equal? (memv 101 '(100 101 102)) '(101 102)))
  )

(define (test-assq)
  (expect '(let ((e '((a 1) (b 2) (c 3)))) (eq? (assq 'a e) (car e))))
  (expect '(let ((e '((a 1) (b 2) (c 3)))) (eq? (assq 'b e) (cadr e))))
  (expect '(let ((e '((a 1) (b 2) (c 3)))) (eq? (assq 'd e) #f)))
  (expect '(let ((e '((a 1) (b 2) (c 3)))) (eq? (assq (list 'a) '(((a)) ((b)) ((c)))) #f)))
  )

(define (test-let-extra)
  (expect '(= (let () 1 2) 2))
  )

(define list-length
  (lambda (obj)
	(call-with-current-continuation
	 (lambda (return)
	   (letrec ((r (lambda (obj)
					 (cond ((null? obj) 0)
						   ((pair? obj)
							(+ (r (cdr obj)) 1))
						   (else (return #f))))))
		 (r obj))))))

(define (test-call/cc)
  (expect '(equal? (call-with-current-continuation
					(lambda (exit)
					  (for-each (lambda (x)
								  (if (negative? x)
									  (exit x)))
								'(54 0 37 -3 245 19))))
				   -3))
  (expect '(equal? (list-length '(1 2 3 4))
				   4))
  (expect '(equal? (list-length '(a b . c))
				   #f))
  )

;; Non standard features
(define (test-match)
  (expect '(equal? ((lambda (x) (match x (0 'zero) (1 'one) (? 'unkown))) 0) 'zero))
  (expect '(equal? ((lambda (x) (match x (0 'zero) (1 'one) (? 'unkown))) 2) 'unkown))
  )

(define (test)
  (test-simple-examples)
  (test-conditionals)
  (test-assignemnt)
  (test-cond)
  (test-case)
  (test-and)
  (test-or)
  (test-let)
  (test-let-extra)
  (test-iteration)
  (test-quasiquote)
  (test-quote)
  (test-internal-defintions)
  (test-eqv?)
  (test-eq?)
  (test-equal?)
  (test-booleans)
  (test-pairs-and-lists)
  (test-symbols)
  (test-characters)
  (test-vectors)
  (test-control-features)
  (test-memq?)
  (test-assq)
  (test-call/cc)
  (test-match)
  (display "tests ok")
  (newline)
  )

(test)

;; (load "r5rs_test.scm")
