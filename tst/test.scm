(define foo (lambda (a b) (lambda (c) (+ a b c))))
(define foo (lambda (a b) (display a)))


(define grr (lambda (a b c) (+ a b c)))

(define foo (lambda (a)
			  (define id (lambda (x) x))
			  (display a)
			  (id 1)))

(define cruel (lambda (a)
				(display a)
				(define id (lambda (a) a))
				(id 2)))

(define (foo a1)
  (define (bar a2)
  (if (null? a2)
      (begin (display "bar1") '())
      (begin (display "bar2") (bar (cdr a2)))))
  (define (baz a3)
    (if (null? a3)
	(begin (display "baz1") '())
	(begin (display "baz2") (baz (cdr a3)))))
  (if  (= (length a1) 2)
       (bar a1)
       (baz a1)))

(define length
  (lambda (li)
    (if (null? li)
	0
	(+ 1 (length (cdr li))))))


(define foo
  (lambda (a1)
    (define bar
      (lambda (a2)
	(if (null? a2)
	    (begin (display "bar1") '())
	    (begin (display "bar2") (bar (cdr a2))))))
    (define baz
      (lambda (a3)
	(if (null? a3)
	    (begin (display "baz1") '())
	    (begin (display "baz2") (baz (cdr a3))))))
    (if  (= (length a1) 2)
	 (bar a1)
	 (baz a1))))

(define (add2 n) (+ n 2))

(define foo (lambda (x) (define c 1) (+ x c)))

(define (fac n)
  (if (= n 0) 
      1
      (* n (fac (- n 1)))))

(letrec ((f (lambda (n)
	      (if (= n 0) 
		  1
		  (* n (f (- n 1)))))))
  (f 5))

(let loop ()
  (display 'running) (loop))

(letrec ((loop (lambda '() (display (quote running)) (loop)))
	 ((lambda '() (display (quote running)) (loop)))) (loop))

((lambda (loop)
   (set! loop (lambda () 
		(display (quote running)) 
		(loop)))
   (loop)) 
 "*unasigned*")

(define a (lambda () (display (quote running)) 
		  (a)))
    
((lambda (x) (set! x 1) 1) 2)

(define (add4 n)
  (define (inner n) (+ n 4))
  (inner n))

(define (unzip a)
  (define (inner li left right)
    (if (null? li)
	(cons (reverse left) (reverse right))
	(inner (cdr li) 
	       (cons (car (car li)) left)
	       (cons (car(cdr (car li))) right))))
  (inner a '() '()))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (reverse list)
  (define (inner list result)
    (if (null? list) 
	result
	(inner (cdr list) (cons (car list) result))))
  (inner list '()))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (zip a b)
  (if (null? a)
      '()
      (cons (cons (car a) (cons (car b) '())) (zip (cdr a) (cdr b)))))

      
(define (let->combination sexp)
  (display sexp) (newline)
  (if (symbol? (cadr sexp))
      (let ((name (cadr sexp))
	    (bindings (unzip (caddr sexp)))
	    (body (cdddr sexp)))
	(let->combination 
	 (cons 'let 
	       (cons (cons (cons '--internal-function-- 
				 (cons 'lambda 
				       (cons (car bindings) 
					     body)))
			   '())
		     (cons (cons '--internal-function-- 
				 (cdr bindings))
			   '())))))
      (let ((bindings (unzip (cadr sexp)))
	    (body (cddr sexp)))
	(cons (cons 'lambda (cons (car bindings) body)) (cdr bindings)))))


(define (let? exp) 
  (if (tagged-list? exp 'let)
      (if (pair? (cadr exp))
	  #t
	  #f)
      #f))

(define (let->combination exp)
  ((lambda (bindings body) 
     (cons (cons 'lambda (cons (car bindings) body))
	   (cdr bindings)))
   (unzip (cadr exp)) 
   (cddr exp)))

(define l '(let ((a 'foo) (b 'bar))
	     (display a) 
	     (display b)))

(define (mul2 n)
  (let ((two 2))
    (* n two)))

(define (loop n i)
  (if (< i n)
      (begin
	(display i)
	(newline)
	(loop n (+ i 1)))
      i))

(define (tst n)
  (if (< n 10)
      (display n)))

;; no garbage collection for ever 10775
(define (for-ever i)
  (display "for-ever:  ") (display i)
  (newline)
  (for-ever (+ i 1)))

(define (print arg . args)
  (display arg)
  (newline)
  (display args))

(define (p . args)
  (display args))


(define (map-vals vals len)
  (define (inner vals len i)
    (cond ((= len i) (cons vals '()))
	  (else (cons (car vals) (inner (cdr vals) len (+ i 1))))))
  (cond ((= (length vals) (- len 1)) (append vals '()))
	((= (length vals) len) vals)
	((> (length vals) len) (inner vals len 1))
	(else (error "Wrong number of values"))))


(define (construct label . data) (cons label data))

(define (depth tree)
  (case (car tree)
    ((tree) ( + 1 (max (depth (cadr tree)) (depth (caddr tree)))))
    ((leaf) 0)
    (else (display (car tree)) (newline) (error "wrong match"))))

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (find-prime-from n)
  (if (fast-prime? n 100)
      n
      (find-prime-from (+ n 1))))

(define (prompt-for-input string) 
  (display string))

(define (list arg . args)
  (if (null? args)
      (cons arg args)
      (cons arg (apply list args))))

(define (assq a li)
  (cond ((null? li) '())
	((not (pair? li)) 
	 (error "ASSQ: argument must be a list"))
	((not (pair? (car li))) 
	 (error "ASSQ: argument must be a list of pairs"))
	((eq? (caar li) a) (car li))
	(else (assq a (cdr li)))))
	 
(define (memq a li)
  (cond ((null? li) #f)
	((eq? a (car li)) li)
	(else (memq a (cdr li)))))

(define (case-key exp) (cadr exp))
(define (case-clauses exp) (cddr exp))
(define (case-clause-datum clause) (car clause))
(define (case-clause-exp clause) (cdr clause))
(define (last-clause? clause) (null? (cdr clause)))
(define (else-clause? clause) (eq? (case-clause-datum clause) 'else))

(define (construct-case-clauses clauses)
  (define (inner clauses)
    (cond ((null? clauses) '())
	  ((and (not (last-clause? clauses)) (else-clause? (car clauses)))
	   (error "CASE: else must be the last clause"))
	  ((else-clause? (car clauses)) clauses)
	  (else ((lambda (c)		; a translated le
		   (cons
		    (cons 
		     (cons 'memv 
			   (cons 'key 
				 (cons (cons 'list 
					     (case-clause-datum c))
				       '())))
		     (case-clause-exp c))
		    (inner (cdr clauses))))
		   (car clauses)))))
  (if (or (null? clauses) 
	  (not (> (length clauses) 1)))
      (begin (display clauses) (error "CASE: must contain at least one clause"))
      (cons 'cond (inner clauses))))

(define (case->cond exp)
  (cons 'let (cons (cons (cons 'key 
			       (cons (case-key exp)'()))
			 '())
		   (cons (construct-case-clauses (case-clauses exp)) 
			 '()))))

(define c '(case c
	     ((#\space #\ht #\cr #\lf) #t)
	     (else #f)))

(define (number->string n)
  (define (inner n chars)
    (if (= n 0)
	(list->string (reverse chars))
	(inner (/ n 10) (cons (integer->char (remainder n 10) chars)))))
  (inner n '()))

(define (le x)
  (match x
	 (() 0)
	 ((?hd . ?tl) (+ 1 (le tl)))
	 (? (display 'foo) (newline) (error "asdasd"))))

(define (foo clauses)
  (and (not (last-clause? clauses)) (else-clause? (car clauses))))

(define (aa a) a)
(define (bb b) b)

(define (cc c) (or (aa c) (bb c)))

(let ((key (* 2 3))) 
  (cond ((memv key '(2 3 5 7)) 'prime) 
	((memv key '(1 4 6 8 9)) 'composite)))

(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite))

(case 1
  ((2) 'two)
  ((1) 'one))

(case (car '(c d))
  ((a) 'a)
  ((b) 'b))

(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else 'consonant))

(define (unquote? exp) (tagged-list? exp 'unquote))
(define (unquote-value exp) (cadr exp))
(define (quasiquote->list exp)
  (cond ((null? exp) '())
	((unquote? exp) (unquote-value exp))
	((symbol? exp) (cons 'quote (cons exp '())))
	((pair? exp) 
	 (cons 'cons 
	       (cons (quasiquote->list (car exp))
		     (cons (quasiquote->list (cdr exp)) 
			   '()))))
	(else exp)))

(define a '(list ,(+ 1 2) 4))
(define b '(quote ,x))
(define c '(list ,x ',x))

(define (make-unasigned-vars vars)
  (if (null? vars)
      '()
      (cons (cons (car vars)
		  (cons "*unasigned*" '()))
	    (make-unasigned-vars (cdr vars)))))

(define (make-var-set-values vars vals)
  (if (null? vars)
      '()
      (cons (cons 'set! (cons (car vars) (cons (car vals) '())))
	    (make-var-set-values (cdr vars) (cdr vals)))))

(define f '(define (letrec->let sexp)
	     (let ((bindings (unzip (cadr sexp)))
		   (body (cddr sexp)))
	       (let ((vars (car bindings))
		     (vals (cdr bindings)))
		 (cons 'let 
		       (cons (make-unasigned-vars vars) 
			     (append (make-var-set-values vars vals) body)))))))

(define (letrec->let sexp) 
  ((lambda (bindings body) 
     ((lambda (vars vals) 
	(cons 'let (cons (make-unasigned-vars vars) 
			 (append (make-var-set-values vars vals) body)))) 
      (car bindings) (cdr bindings))) 
   (unzip (cadr sexp)) (cddr sexp)))

(define (named-let? sexp)
  (and (pair? sexp) 
       (> (length sexp) 3) 
       (eq? (car sexp) 'let) 
       (symbol? (cadr sexp))))

(define a '(define (named-let->letrec sexp)
	     (let ((name (cadr sexp))
		   (bindings (unzip (caddr sexp)))
		   (body (cdddr sexp)))
	       (let ((vars (car bindings))
		     (vals (cdr bindings)))
		 (cons 'letrec 
		       (cons (cons 
			      (cons name 
				    (cons (cons 'lambda 
						(cons vars 
						      body))
					  '()))
			      '())
			     (cons (cons name 
			      vals) 
				   '())))))))

(let fac ((x 5)) 
  (if (= x 0) 1 
      (* x (fac (- x 1)))))

(define (let*->nested-let sexp)
  (define (inner bindings body)
    (if (= (length bindings) 1)
	(cons 'let 
	      (cons (cons (car bindings) '())
		    body))
	(cons 'let 
	      (cons (cons (car bindings) '())
		    (cons (inner (cdr bindings) body) 
			  '())))))
  (inner (cadr sexp) (--internal-translate-impl-- (cddr sexp))))


(let*->nested-let '(let* ((a 1) (b a)) b))
(let*->nested-let '(let* ((a 1) (b a)) 6 5 43 b))

(let* ((a 1) (b a)) b)

(let ((a 1))
  (let ((b a)) b))

(let ()
  1 2)

(letrec ((a 1)) a)
(let ((a 'foo)) a)
    
(define a '(or (= 1 2) (= 1 1)))

(or (= 1 2) (= 1 1)))
(let ((x (= 1 2)))
  (if x x (let ((x (= 1 3))) (if x x #f))))

(define b '(define (foo y)
	     (or (= y 1) (= y 2) (= y 3))))
(define c '(define (bar x)
	     (cond ((or (= x 1) (= x 2)) #t)
		   (else #f))))

(define (translate exp)
  (define (tagged-list? exp tag)
    (if (pair? exp)
	(eq? (car exp) tag)
	#f))
  (define (unzip a)
    (define (inner li left right)
      (if (null? li)
	  (cons (reverse left) (reverse right))
	  (inner (cdr li) 
		 (cons (car (car li)) left)
		 (cons (car(cdr (car li))) right))))
    (inner a '() '()))
  (define (or? exp) (tagged-list? exp 'or))
  (define (or->if exp)
    (define (inner operands)
      (if (null? operands)
	  #f
	  (list (list 'lambda (list '--internal-or-var--) 
		      (list 'if '--internal-or-var-- '--internal-or-var-- 
			    (inner (cdr operands))))
		(car operands))))
    (inner (cdr exp)))
    (define (and? exp) (tagged-list? exp 'and))
  (define (and->if exp)
    (define (inner operands)
      (display operands) (newline)
      (if (null? operands)
	  #t
	  (if (null? (cdr operands))
	      (list 'if (car operands) (car operands) #f)
	      (list 'if (car operands) (inner (cdr operands)) #f))))
    (inner (cdr exp)))
  (cond  ((not (pair? exp)) exp)
	 ((or? exp) (or->if exp))
	 ((and? exp) (and->if exp))
	 ((let? exp) (let->combination exp))
	 ((pair? exp) (cons (translate (car exp)) 
			     (translate (cdr exp))))))

(define (named-let->letrec sexp) 
  ((lambda (name bindings body) 
     ((lambda (vars vals) 
	(cons 'letrec 
	      (cons (cons 
		     (cons name 
			   (cons (cons 'lambda 
				       (cons vars 
					     body)) 
				 '())) 
		     '()) 
		    (cons (cons name vals) '()))))
      (car bindings) 
      (cdr bindings)))
   (cadr sexp) 
   (unzip (caddr sexp)) 
   (cdddr sexp)))

(define (tag-backquote? x) (tagged-list? x 'quasiquote))
(define (tag-comma? x) (tagged-list? x 'unquote))
(define (tag-comma-atsign? x) (tagged-list? x 'unquote-splicing))
(define (tag-data x) (cadr x))

(define (qq-expand x)
  (cond ((tag-comma? x) (tag-data x))
	((tag-comma-atsign? x) (error "QQ-EXPAND: illegal expression"))
	((tag-backquote? x) (qq-expand (qq-expand (tag-data x))))
	((pair? x) (quasiquote (append (unqote (qq-expand-list (car x)))
				       (unquote (qq-expand-list (cdr x))))))
	(else (quasiquote (quote (unquote x))))))

(define (qq-expand-list x)
  (cond ((tag-comma? x) (quasiquote (list ,(tag-data x))))
	((tag-comma-atsign? x) (tag-data x))
	((tag-backquote? x) (qq-expand-list (qq-expand (tag-data x))))
	((pair? x) 
	 (quasiquote (list (append (unquote(qq-expand-list (car x)))
				   (unquote (qq-expand-list (cdr x))))))
	 (else (quasiquote (quote (unquote x)))))))

(define (qq->expanded x) (qq-expand (tag-data x)))

(define s '(quasiquote x))


(let* ((a 2)
       (b (+ a 7)))
  b)


(define (improper-list? l)
  (cond ((null? l) #f)
	((pair? l) (improper-list? (cdr l)))
	(else #t)))

(define (improper-list-length l)
  (if (pair? l) 
      (+ 1 (improper-list-length (cdr l)))
      0))


(define (define? exp) (tagged-list? exp 'define))

(define (define->lambda exp)
  (if (not (symbol? (car (cdr exp))))
      (cons 'define 
	    (cons (car (car (cdr exp)))
		  (cons (cons 'lambda
			      (cons (cdr (car (cdr exp)))
				    (cdr (cdr exp))))
			'())))
      exp))

(define a '(define (foo)
	     (define a (lambda () 1))
	     (define b (lambda () 1))
	     (+ a b)))

(define b '(define a (lambda () 1)))

;; Defnitions may occur at the beginning of a body (that
;; is, the body of a lambda, let, let*, letrec, let-syntax,
;; or letrec-syntax expression or that of a defnition of an
;; appropriate form).

(define (lambda-define? exp) 
  (and (tagged-list? exp 'define) 
       (tagged-list? (caddr exp) 'lambda)))

(define (get-defines body defines rest)
  (cond ((null? body) (cons defines rest))
	((not (define

(define (inner-def->let body defines)
  (if (null? body)

(define (foo)
  (let ((a (lambda (x) (display x)))
	(b (lambda (y) (+ y 1))))
    (a (b 1))))

;; translating cond expressions to if expressions
(define (mk-if predicate consequent alternative)
  (cons 'if
	(cons (predicate
	       (cons consequent
		     (cons alternative '()))))))

(define (mk-begin exp) (cons 'begin exp))
(define (last-exp? exp) (null? (cdr exp)))
(define (first-exp exp) (car exp))

(define (sequence->exp exp)
  (if (null? exp)
      '()
      (if (last-exp? exp)
	  (first-exp exp)
	  (mk-begin exp))))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (expand-clauses exp)
  (if (null? exp)
      #f
      (if (cond-else-clause? (car exp))
	  (if (null? (cdr exp))
	      (sequence->exp (cond-actions (car exp)))
	      (error "COND: else is not last in cond."))
	  (mk-if (cond-predicate (car exp))
		 (sequence->exp (cond-actions (car exp)))
		 (expand-clauses (cdr exp))))))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (lst . args) args)
  
(define id (lambda (x) x))

(define fac
  (lambda (x)
	(if (equal? x 1) 1
		(* x (fac (- x 1))))))

(define (foo v) v)
((lambda (x) x) 1)

(define (foo)
  (define (bar)
	(let ((a 1))
	  (cond ((= a 1)
			 (set! a 2)
			 (set! a 3)))
	  a))
  (bar))

(let ((a 1)) (cond ((= a 1) (set! a 2) (set! a 3))) a)

(define a 1)

(begin (set! a 2) (set! a 3)) 


(define (parse value)
  (call-with-current-continuation
   (lambda (throw)
	 (if value
		 (throw 'true)
		 'false))))

(call-with-current-continuation
 (lambda (exit)
   (for-each (lambda (x)
			   (if (negative? x)
				   (exit x)))
			 '(54 0 37 -3 245 19))))

(for-each (lambda (x) (display x)) '(1 2))

(define (a-stream)
  (define (next n)
	(cons n (delay (next (+ n 1)))))
  (next 0))

(define a-stream (letrec ((next (lambda (n) (cons n (delay (next (+ n 1)))))))
						  (next 0)))
