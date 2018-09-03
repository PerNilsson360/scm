;; First order rewriting

(load "util.scm")

(define rule-context '())
(define constant-context '())
(define (add-rule rule) (set! rule-context (cons rule rule-context)))
(define (add-constant constant)
  (set! constant-context (cons constant constant-context)))

;; constants are symbols that have been declared
(define (constant? exp) (memq exp constant-context))

;; a constant declaration is a list starting with
;; the keyword "define" and containg the constant
;; either a proper constant or a function declararion
(define (constant-definition? exp)
  (and (tagged-list? exp 'define) (= 2 (length exp))))
(define (definition-name exp)
  (let ((body (cadr exp)))
    (if (pair? body)
	(car body)
	body)))

;; Rules a are also define using the "define" keyword, followed
;; by the declaration of the left part which rewrites to the right
;; part
(define (rule-definition? exp)
  (and (tagged-list? exp 'define) (= 3 (length exp))))
(define (rule-body exp) (cdr exp))

;; variables are just symbols that are not constants
(define (mk-variable name) name)
(define (variable? exp) (and (symbol? exp) (not (constant? exp))))
(define (variable-name exp) exp)

;; A term is constant symbol with 0 .. n arguments
;; A constant symbol that takes more than 0 arguments
;; is also called a function
(define (mk-term name terms)
  (if (null? terms)
      name
      (cons name terms)))
(define (term? exp)
  (if (pair? exp)
      (if (not (constant? (car exp)))
	  (error "TERM?: need to declare function" (car exp)))
      (constant? exp)))
(define (term-name exp) (if (pair? exp) (car exp) exp))
(define (term-args exp) (if (pair? exp) (cdr exp) '()))

;; A substitution is an association list of  following form ((x1, t1) ...) 
(define (in-domain? var subst) (pair? (assoc var subst)))

;; Apply a substition to a variable i.e replace the variable
;; with the value that the substitution defines.
(define (app subst var)
  (let ((res (assoc var subst)))
    (if (null? res)
	(error "APP: var must be in domain of subst")
	(cadr res))))

;; lift applies a substitution to a term
;; which can either be a variable or a proper term
(define (lift subst term)
  (cond ((variable? term)
	 (let ((var-name (variable-name term)))
	   (if (in-domain? var-name subst)
	       (app subst var-name)
	       term)))
	((term? term)
	 (let ((name (term-name term))
	       (args (term-args term)))
	   (mk-term
	    name
	    (map (lambda (arg) (lift subst arg)) args))))
	(else (error "LIFT: wrong term type: " term))))

;; Matches takes a list (pat, objs) and a result substition
;; which initialy is empty. It returns a substitution that
;; solves the matching problem. It returns (none) on
;; failure and (some result) on succes
(define (matches problem result)
  (if (null? problem)
      (mk-some result)
      (let* ((first (car problem))
	     (pat (car first))
	     (obj (cadr first)))
	(cond ((variable? pat)
	       (let ((name pat))
		 (if (in-domain? name result)
		     (if (equal? (app result name) obj)
			 (matches (cdr problem) result)
			 (mk-none))
		     (matches (cdr problem)
			      (cons (list pat obj) result)))))
	      ((variable? obj) (mk-none))
	      ((and (term? pat) (term? obj))
	       (let ((pat-name (term-name pat))
		     (obj-name (term-name obj)))
		 (if (equal? pat-name obj-name)
		     (matches (append (zip (term-args pat)
					   (term-args obj))
				      (cdr problem))
			      result)
		     (mk-none))))
	      (else (error "MATCHES: wrong term types" pat obj))))))
  
(define (pattern-match pat obj) (matches (list (list pat obj)) '()))

;; Rewrite takes a list of rewrite rules and a term as arguments
;; It tries to do a single step rewrite, If no rule
;; has been applied (none) is returned otherwise (some result)
;; is returned
(define (rewrite rules term)
  (if (null? rules)
      (mk-none)
      (let* ((first (car rules))
	     (left (car first))
	     (right (cadr first))
	     (result (pattern-match left term)))
	(if (none? result)
	    (rewrite (cdr rules) term)
	    (mk-some (lift (some-val result) right))))))
		     
;; Normalize applies rewrite rules until no more rules can be applied
(define (normalize rules term)
  (cond ((variable? term) term)
	((term? term)
	 (let* ((name (term-name term))
		(terms (term-args term))
		(u (mk-term
		    name
		    (map (lambda (term) (normalize rules term))
			 terms)))
		(result (rewrite rules u)))
	   (if (none? result)
	       u
	       (normalize rules (some-val result)))))
	(else (error "NORMALIZE: wrong term type" term))))

(define (evaluate exp)
  (cond ((constant-definition? exp)
	 (add-constant (definition-name exp)))
	((rule-definition? exp)
	 (add-rule (rule-body exp)))
	((variable? exp) exp)
	((term? exp) (normalize rule-context exp))
	(else (error "EVAL: unkown expression" exp))))

(define (display-prompt) (display '=>))

(define (read-eval-print)
  (display-prompt)
  (let* ((input (read))
	 (output (evaluate input)))
    (display output)
    (newline)
    (read-eval-print)))

(read-eval-print)
