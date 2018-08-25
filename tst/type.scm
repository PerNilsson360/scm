(define at '(deftyp (fac number) number))
(define ax '(define (fac x) (if (= x 1) 1 (* x (fac (- x 1))))))

(define bt '(deftyp x number))
(define bx '(define x 1))

(define cx '(+ 1 1))
(define dx '(* 1 2))

(define et '(deftyp (add2 number) number))
(define ex '(define (add2 x) (+ x 2)))

;; type record 
;; name:symbol * function:bool * arg-types:symbol list * result-type:symbol
(define type-env '())

(define (type-env-find name env) 
  (match (assoc name env)
	 (#f #f) 
	 (?tr (cadr tr))))

(define (type-env-add name type tenv) 
  (let ((t (type-env-find name type-env)))
    (if t
	(set-cdr! t (list type))
	(set! type-env (cons (list name type) type-env)))))

;; returns the suplied env with added name type pairs
(define (type-env-extend names types tenv)
  (match (cons names types)
	 (('() . '()) tenv)
	 (((?n . ?ns) . (?t . ?ts))
	  (cons (list n t) (type-env-extend ns ts tenv)))
	 (? (display "wrong number of function parameters"))))

(define (mk-type-record name function arg-types result-type)
  (list name function arg-types result-type))

(define (eval-def-type exp env type-env)
  (match exp
	 ((deftyp (?fun . ?arg-types) ?res-type) 
	  (type-env-add fun 
			(mk-type-record fun #t arg-types res-type) 
			type-env))
	 ((deftyp ?var ?type) 	  
	  (type-env-add var (mk-type-record var #f '() type) type-env))
	 (?e (error "EVAL-DEF-TYPE: bad exp: " e))))

(define (load-type-env)
  (let ((prelude-types 
	 '((deftyp (+ number number) number)
	   (deftyp (* number number) number))))
    (for-each (lambda (def) (eval-def-type def '() type-env)) prelude-types)))

;; type checking of a expression
(define (type-check exp tenv)
  (begin (display exp) (newline)
	 (match exp
		((?def (?fun . ?args) ?body) 
		 (type-check-fun-def fun args body tenv))
		((?def ?var ?value) (error "not impl 2"))
		((?rand . ?rators) (type-check-app rand rators tenv))
		(?value (cond ((number? value) 'number)
			      (else 'none))))))

(define (type-check-fun-def fun args body tenv)
  (let ((trec (type-env-find fun tenv)))
    (match trec
	   ((? #t ?argty ?resty)
	    (if (eq? (length args) (length argty))
		(let ((ext-tenv (type-env-extend args argty tenv)))
		  (type-check body tenv))
		(else (begin (display "wrong number of arguments in fun") 
			     'none)))))))

(define (type-check-app rand rators tenv)
  (let ((type (type-env-find rand tenv)))
    (match type
	   ((?name #t ?argty ?resty) 
	    (if (type-check-app-rators rators argty tenv)
		resty
		'none))
	   (? (begin (display "could not find type for app") (newline)
		     'none)))))

(define (type-check-app-rators rators argty tenv)
  (match (cons rators argty)
	 (('() . '()) #t)
	 (('() . ?t) (begin (display "to few arguments") #f))
	 (('t . '()) (begin (display "to many arguments") #f))
	 (((?r . ?rs) . (?at . ?ats)) 
	  (let ((rt (type-check r tenv)))
	    (if (eq? rt at)
		(type-check-app-rators rs ats tenv)
		(begin (display "type missmatch in app") (newline) 'none))))))

(define (foo x)
  (match x
	 ((define . ?b) (display b))
	 (?a (display 'darn))))

(define m '(match x
		  ((define . ?b) (display b))
		  (?a (display 'darn))))

(match 'defi
       (define #t)
       (? 'darn))
