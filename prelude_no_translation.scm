(define caar (lambda (sexp) (car(car sexp))))
(define cadr (lambda (sexp) (car(cdr sexp))))
(define caadr (lambda (sexp) (car(car(cdr sexp)))))
(define caddr (lambda (sexp) (car(cdr(cdr sexp)))))
(define cddr (lambda (sexp) (cdr (cdr sexp))))
(define cdar (lambda (sexp) (cdr(car sexp))))
(define cdadr (lambda (sexp) (cdr(car(cdr sexp)))))
(define cdddr (lambda (sexp) (cdr(cdr(cdr sexp)))))
(define cadddr (lambda (sexp) (car(cdr(cdr(cdr sexp))))))

(define internal-list 
  (lambda (a . args)
    (if (null? args)
	(cons a '())
	(cons a (apply internal-list args)))))
  
(define accumulate 
  (lambda (f identity list)
    (if (null? list) 
	identity
	(f (car list) (accumulate f identity (cdr list))))))

(define internal-append 
  (lambda (a . args)
    (define inner 
      (lambda (left right)
	(if (null? left) 
	    right
	    (if (null? right) 
		left
		(cons (car left) (inner (cdr left) right))))))
    (accumulate inner '() (cons a args))))

(define reverse 
  (lambda (list)
    (define inner 
      (lambda (in out)
	(if (null? in) 
	    out
	    (inner (cdr in) (cons (car in) out)))))
    (inner list '())))

(define unzip 
  (lambda (a)
    (define inner 
      (lambda (li left right)
	(if (null? li)
	    (cons (reverse left) (reverse right))
	    (inner (cdr li) 
		   (cons (car (car li)) left)
		   (cons (car(cdr (car li))) right)))))
    (inner a '() '())))

;; translation of library syntax expressions to syntax expressions
(define __internal-translate__
  (lambda (exp)
    ((lambda (exp)
       ;(display exp) 
       ;(newline)
       exp)
     (__internal-translate-impl__ exp))))

(define tagged-list? 
  (lambda (exp tag)
    (if (pair? exp)
	(eq? (car exp) tag)
	#f)))
(define or? (lambda (exp) (tagged-list? exp 'or)))
(define or->if 
  (lambda (exp)
    (define inner 
      (lambda (operands)
	(if (null? operands)
	    #f
	    (internal-list (internal-list 'lambda (internal-list '__internal-or-var__) 
					  (internal-list 'if '__internal-or-var__ '__internal-or-var__ 
							 (inner (cdr operands))))
			   (car operands)))))
    (inner (cdr exp))))
(define and? (lambda (exp) (tagged-list? exp 'and)))
(define and->if 
  (lambda (exp)
    (define inner 
      (lambda (operands)
	(if (null? operands)
	    #t
	    (if (null? (cdr operands))
		(internal-list 'if (car operands) (car operands) #f)
		(internal-list 'if (car operands) (inner (cdr operands)) #f)))))
    (inner (cdr exp))))
;; let expressions
(define let? (lambda (exp) (tagged-list? exp 'let)))
;; translates let to lambda
(define let->combination 
  (lambda (sexp) 
    ((lambda (bindings body) 
       (cons (cons 'lambda (cons (car bindings) body)) 
	     (cdr bindings)))
     (unzip (cadr sexp)) 
     (__internal-translate-impl__ (cddr sexp))))) ; need to translate the body
;; named let
(define named-let? 
  (lambda (exp)
    (if (tagged-list? exp  'let)
	(if (symbol? (cadr exp))
	    #t
	    #f)
	#f)))
(define named-let->letrec 
  (lambda (sexp) 
    (__internal-translate-impl__ 	; need to translate the letrec
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
      (__internal-translate-impl__ (cdddr sexp))))))
;; let*
(define let*? (lambda (exp) (tagged-list? exp 'let*)))
(define let*->nested-let 
  (lambda (sexp)
    (define inner 
      (lambda (bindings body)
	(if (= (length bindings) 1)
	    (cons 'let 
		  (cons (cons (car bindings) '())
			body))
	    (cons 'let 
		  (cons (cons (car bindings) '())
			(cons (inner (cdr bindings) body) 
			      '()))))))
    (__internal-translate-impl__ 
     (inner (cadr sexp) (__internal-translate-impl__ (cddr sexp))))))
;; letrec
(define letrec? (lambda (exp) (tagged-list? exp 'letrec)))
(define make-unasigned-vars 
  (lambda (vars)
    (if (null? vars)
	'()
	(cons (cons (car vars)
		    (cons "*unasigned*" '()))
	      (make-unasigned-vars (cdr vars))))))
(define make-var-set-values 
  (lambda (vars vals)
    (if (null? vars)
	'()
	(cons (cons 'set! (cons (car vars) (cons (car vals) '())))
	      (make-var-set-values (cdr vars) (cdr vals))))))
(define letrec->let 
  (lambda (sexp) 
    (__internal-translate-impl__ 	; need to translate the let
     ((lambda (bindings body) 
	((lambda (vars vals) 
	   (cons 'let (cons (make-unasigned-vars vars) 
			    (internal-append (make-var-set-values vars vals) body)))) 
	 (car bindings) (cdr bindings))) 
      (unzip (cadr sexp)) 
      (__internal-translate-impl__ (cddr sexp))))))
;; case
(define case? (lambda (exp) (tagged-list? exp 'case)))
(define case-key (lambda (exp) (cadr exp)))
(define case-clauses (lambda (exp) (cddr exp)))
(define case-clause-datum (lambda (clause) (car clause)))
(define case-clause-exp (lambda (clause) (cdr clause)))
(define last-clause? (lambda (clause) (null? (cdr clause))))
(define else-clause? (lambda (clause) (eq? (case-clause-datum clause) 'else)))
(define construct-case-clauses 
  (lambda (clauses)
    (define inner 
      (lambda (clauses)
	(if (null? clauses) 
	    '()
	    (if (else-clause? (car clauses))
		(if (not (last-clause? clauses)) 
		    (error "CASE: else must be the last clause")
		    clauses)
		((lambda (c)		; translated to let
		   (cons
		    (cons 
		     (cons 'memv 
			   (cons 'key 
				 (cons (internal-list 'quote 
						      (case-clause-datum c))
				       '())))
		     (case-clause-exp c))
		    (inner (cdr clauses))))
		 (car clauses))))))
    (if (null? clauses) 
	(begin (display clauses) (error "CASE: must contain at least one clause"))
	(if (not (> (length clauses) 0))
	    (begin (display clauses) 
		   (error "CASE: must contain at least one clause"))
	    (cons 'cond (inner clauses))))))
(define case->cond 
  (lambda (exp)
    (__internal-translate-impl__ 	; needed sinc translating to let
     (cons 'let (cons (cons (cons 'key 
				  (cons (case-key exp)'()))
			    '())
		      (cons (construct-case-clauses (case-clauses exp)) 
			    '()))))))
;; quasiquote
(define quasiquote? (lambda (exp) (tagged-list? exp 'quasiquote)))
(define unquote-splicing? (lambda (exp) (tagged-list? exp 'unquote-splicing)))
(define unquote? (lambda (exp) (tagged-list? exp 'unquote)))
(define unquote-value (lambda (exp) (cadr exp)))
(define quasiquote->list 
  (lambda (exp)
    (cond ((null? exp) '())
	  ((unquote? exp) (unquote-value exp))
	  ((symbol? exp) (cons 'quote (cons exp '())))
	  ((pair? exp) 
	   (if (unquote-splicing? (car exp))
	       (cons 'append 
		     (cons (unquote-value (car exp))
			   (cons (quasiquote->list (cdr exp))
				 '())))
	       (cons 'cons 
		     (cons (quasiquote->list (car exp))
			   (cons (quasiquote->list (cdr exp)) 
				 '())))))
	  (else exp))))
;; define
(define define? 
  (lambda (exp) (if (tagged-list? exp 'define)
		    (if (not (symbol? (car (cdr exp))))
			#t
			#f)
		    #f)))

(define define->lambda 
  (lambda (exp)
    (cons 'define 
	  (cons (car (car (cdr exp)))
		(cons (cons 'lambda
			    (cons (cdr (car (cdr exp)))
				  (__internal-translate-impl__ (cdr (cdr exp)))))
		      '())))))

(define quote? (lambda (exp) (tagged-list? exp 'quote)))

;; do not translate the pattern matching constructs
;; since it should be possible to match on key words
(define match? (lambda (exp) (tagged-list? exp 'match)))
(define match-key (lambda (exp) (car (cdr exp))))
(define match-clauses (lambda (exp) (cdr (cdr exp))))
(define translate-match-clauses 
  (lambda (clauses)
    (if (null? clauses)
	'()
	(cons ((lambda (clause) 		;translated let
		 (if (< (length clause) 2)
		     (error "to few elements in match clause" clause)
		     (cons (car clause) 
			   (__internal-translate-impl__ (cdr clause)))))
	       (car clauses))
	      (translate-match-clauses (cdr clauses))))))
(define translate-match 
  (lambda (exp)
      (if (< (length exp) 3)
	  (error "Missing clauses in match" exp)
	  (cons 'match
		(cons (match-key exp) 
		      (translate-match-clauses (match-clauses exp)))))))
					       
(define __internal-translate-impl__ 
  (lambda (exp)
    (cond ((quote? exp) exp) 
	  ((not (pair? exp)) exp)
	  ((or? exp) (or->if exp))
	  ((and? exp) (and->if exp))
	  ((define? exp) (define->lambda exp))
	  ((named-let? exp) (named-let->letrec exp))
	  ((let? exp) (let->combination exp))
	  ((let*? exp) (let*->nested-let exp))
	  ((letrec? exp) (letrec->let exp))
	  ((case? exp) (case->cond exp))
	  ((quasiquote? exp) (quasiquote->list (cdr exp)))
	  ((match? exp) (translate-match exp))
	  ((pair? exp) (cons (__internal-translate-impl__ (car exp)) 
			     (__internal-translate-impl__ (cdr exp)))))))
