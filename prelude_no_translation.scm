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

;; translation of library syntax expressions to syntax expressions
(define __internal-translate__
  (lambda (exp)
    ((lambda (exp)
       ;(display exp)(newline)
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
	    (list (list 'lambda (list '__internal-or-var__) 
					  (list 'if '__internal-or-var__ '__internal-or-var__ 
							 (inner (cdr operands))))
			   (__internal-translate-impl__ (car operands))))))
    (inner (cdr exp))))
(define and? (lambda (exp) (tagged-list? exp 'and)))
(define and->if 
  (lambda (exp)
    (define inner 
      (lambda (operands)
	(if (null? operands)
	    #t
	    (if (null? (cdr operands))
		(list 'if (__internal-translate-impl__ (car operands)) (__internal-translate-impl__ (car operands)) #f)
		(list 'if (__internal-translate-impl__ (car operands)) (inner (cdr operands)) #f)))))
    (inner (cdr exp))))
;; let expressions
(define let? (lambda (exp) (tagged-list? exp 'let)))
;; translates let to lambda
(define let->combination 
  (lambda (sexp)
    (if (null? (cadr sexp))
	(caddr sexp)
	((lambda (bindings body) 
	   (cons (cons 'lambda (cons (car bindings) body)) 
		 (__internal-translate-impl__ (cdr bindings))))
	 (unzip (cadr sexp)) 
	 (__internal-translate-impl__ (cddr sexp))))))
;; named let
(define named-let? 
  (lambda (exp)
    (if (tagged-list? exp  'let)
	(symbol? (cadr exp))
	#f)))
(define named-let->letrec 
  (lambda (sexp) 
    (__internal-translate-impl__ 	; need to translate the letrec
     ((lambda (name bindings body) 
	((lambda (vars vals) 
	   (cons 'letrec 
		 (cons (list 
			(cons name 
			      (list (cons 'lambda 
					  (cons vars 
						body)))))
		       (list (cons name vals)))))
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
		  (cons (list (car bindings))
			body))
	    (cons 'let 
		  (cons (list (car bindings))
			(list (inner (cdr bindings) body)))))))
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
				 (cons (list 'quote 
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
    (if (null? exp)
	'()
	(if (unquote? exp)
	    (unquote-value exp)
	    (if (symbol? exp)
		(cons 'quote (cons exp '()))
		(if (pair? exp) 
		    (if (unquote-splicing? (car exp))
			(cons 'append 
			      (cons (unquote-value (car exp))
				    (cons (quasiquote->list (cdr exp))
					  '())))
			(cons 'cons 
			      (cons (quasiquote->list (car exp))
				    (cons (quasiquote->list (cdr exp)) 
					  '()))))
		    exp))))))
;; define
(define define? 
  (lambda (exp)
    (if (tagged-list? exp 'define)
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

;; translating cond expressions to if expressions
(define cond? (lambda (exp) (tagged-list? exp 'cond)))
(define cond->if
  (lambda (exp)
    (define mk-if
      (lambda (predicate consequent alternative)
	(cons 'if
	      (cons predicate
		    (cons consequent
			  (cons alternative '()))))))
    (define mk-begin (lambda (exp) (cons 'begin exp)))
    (define last-exp? (lambda (exp) (null? (cdr exp))))
    (define first-exp (lambda (exp) (car exp)))
    (define sequence->exp
      (lambda (exp)
	(if (null? exp)
	    '()
	    (if (last-exp? exp)
		(__internal-translate-impl__ (first-exp exp))
		(mk-begin (__internal-translate-impl__ exp))))))
    (define cond-clauses (lambda (exp) (cdr exp)))
    (define cond-predicate (lambda (clause) (car clause)))
    (define cond-actions (lambda (clause) (cdr clause)))
    (define cond-else-clause?
      (lambda (clause)
	(eq? (cond-predicate clause) 'else)))
    (define expand-clauses
      (lambda (exp)
	(if (null? exp)
	    #f
	    (if (cond-else-clause? (car exp))
		(if (null? (cdr exp))
		    (sequence->exp (cond-actions (car exp)))
		    (error "COND: else is not last in cond."))
		(mk-if (__internal-translate-impl__ (cond-predicate (car exp)))
		       (sequence->exp (cond-actions (car exp)))
		       (__internal-translate-impl__ (expand-clauses (cdr exp))))))))
    (expand-clauses (cond-clauses exp))))

;; External interface procedure
(define __internal-translate-impl__ 
  (lambda (exp)
    (if (quote? exp)
	exp
	(if (not (pair? exp))
	    exp
	    (if (or? exp)
		(or->if exp)
		(if (and? exp)
		    (and->if exp)
		    (if (define? exp)
			(define->lambda exp)
			(if (named-let? exp)
			    (named-let->letrec exp)
			    (if (let? exp)
				(let->combination exp)
				(if (let*? exp)
				    (let*->nested-let exp)
				    (if (letrec? exp)
					(letrec->let exp)
					(if (cond? exp)
					    (cond->if exp)
					    (if (case? exp)
						(case->cond exp)
						(if (quasiquote? exp)
						    (quasiquote->list (cdr exp))
						    (if (match? exp)
							(translate-match exp)
							(if (pair? exp)
							    (cons (__internal-translate-impl__ (car exp))
								  (__internal-translate-impl__ (cdr exp)))))))))))))))))))
