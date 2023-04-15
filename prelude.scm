(define (list-ref list k)
  (cond ((null? list) (error "LIST-REF: argument out of length"))
	((= k 0) (car list))
	(else (list-ref (cdr list) (- k 1)))))

(define (accumulate f identity list)
  (if (null? list) 
      identity
      (f (car list) (accumulate f identity (cdr list)))))

(define (append . args)
  (define (inner left right)
  (if (null? left) 
      right
      (if (null? right) 
		  left
		  (cons (car left) (inner (cdr left) right)))))
  (accumulate inner '() args))

(define (get-list-of-cars lists result)
  (cond ((null? lists) (reverse result))
	((null? (car lists)) '())
	(else (get-list-of-cars (cdr lists) (cons (caar lists) result)))))

(define (get-list-of-cdrs lists result)
  (cond ((null? lists) (reverse result))
	((null? (car lists)) (error "bad list of list -- GET-LIST-OF-CDRS"))
	(else (get-list-of-cdrs (cdr lists) (cons (cdar lists) result)))))

(define (map proc list . lists)
  (define (inner proc args)
    (let ((cars (get-list-of-cars args '())))
      (if (null? cars)
	  '()
	  (cons (apply proc cars) (inner proc (get-list-of-cdrs args '()))))))
  (inner proc (cons list lists)))
    
(define (for-each proc list . lists)
  (define (inner proc args)
    (let ((cars (get-list-of-cars args '())))
      (if (not (null? cars))
	  (begin (apply proc cars) 
		 (inner proc (get-list-of-cdrs args '()))))))
  (inner proc (cons list lists)))

(define (assq a li)
  (cond ((null? li) #f)
	((not (pair? li)) 
	 (error "ASSQ: argument must be a list"))
	((not (pair? (car li))) 
	 (error "ASSQ: argument must be a list of pairs"))
	((eq? (caar li) a) (car li))
	(else (assq a (cdr li)))))

(define (assoc a li)
  (cond ((null? li) #f)
	((not (pair? li)) 
	 (error "ASSOC: argument must be a list"))
	((not (pair? (car li))) 
	 (error "ASSOC: argument must be a list of pairs"))
	((equal? (caar li) a) (car li))
	(else (assoc a (cdr li)))))

(define (memq a li)
  (cond ((null? li) #f)
	((eq? a (car li)) li)
	(else (memq a (cdr li)))))

(define (memv a li)
  (cond ((null? li) #f)
	((eqv? a (car li)) li)
	(else (memv a (cdr li)))))

(define (member a li)
  (cond ((null? li) #f)
		((equal? a (car li)) li)
		(else (member a (cdr li)))))

(define (load file-name)
  (define (inner port)
    (let ((sexp (read port)))
      (if (not (eof-object? sexp))
	  (begin (newline) 
		 (display sexp) 
		 (newline)
		 (eval sexp environment) 
		 (inner port)))))
  (inner (open-input-file file-name)))

(define (abs a) (if (> a 0) a (* a -1)))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (force l) (l))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

;;insertion sort
(define (sort list pred?)
  (letrec ((insert (lambda (elem list)
		  (cond ((null? list) (cons elem '()))
			((pred? elem (car list)) (cons elem list))
			(else (cons (car list) (insert elem (cdr list)))))))
	(inner (lambda (list result)
		 (if (null? list)
		     result
		     (inner (cdr list) (insert (car list) result))))))
  (inner list '())))

(define (char-alphabetic? c)
  (case c
    ((#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z) #t)
    (else #f)))

(define (char-numeric? c)
  (case c
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) #t)
    (else #f)))

(define (char-whitespace? c)
  (case c
    ((#\space #\ht #\cr #\lf) #t)
    (else #f)))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (blob-u16-ref blob k)
  (let ((a (blob-u8-ref blob k))
	(b (blob-u8-ref blob (inc k))))
    (+ a (* 256 b))))

(define (expt a b)
  (cond ((< b 0) (error "EXPT: second arg needs to > 0"))
	((= b 0) 1)
	((= b 1) a)
	(else (* a (expt a (dec b))))))

;; algebraic data types

(define (check-variant-args preds args)
  (if (not (= (length preds) (length args)))
	  (error "check-variant-args: wrong number of args" args preds)
	  (let loop ((ps preds)
				 (as args)
				 (i 0))
		(if (not (null? ps))
			(if ((eval (car ps) environment) (car as))
				(loop (cdr ps) (cdr as) (+ i 1))
				(error "check-variant-args: wrong argument type" (car as) i))))))

(define (define-datatype name . variants)
  (define (make-pred-name name)
	(string->symbol(string-append(symbol->string name) "?")))
  (define (make-variant-predicate name variants)
	(let ((variant-pred-names (map car variants))
		  (pred-name (make-pred-name name)))
	  (eval `(define (,pred-name arg)
			   (if (pair? arg)
				   (memv (car arg) (quote ,variant-pred-names))
				   #f))
			environment)))
  (define (make-variant-constructor variant)
	(match variant
	  ((?name . ?preds)
	   (eval `(define (,name . args)
				(check-variant-args (quote ,preds) args) 
				(cons (quote ,name) args))
			 environment))
	  (? (error "make-variant-constructor: variant should be a list"))))
  ;; function body
  (make-variant-predicate name variants)
  (let loop ((vs variants))
	(match vs
	  ((?hd . ?tl)
	   (make-variant-constructor hd)
	   (loop tl)))))
		
(define (any? val) #t)

(define-datatype 'option
  '(option-none)
  '(option-some any?))

(define-datatype 'int
  '(z)
  '(s int?))
