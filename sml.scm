;; Implementing a subset of sml. Parsing is done by flex/bison
;; (read-sml) returns list representations of the datatypes below

(define-datatype 'type
  '(int-type)
  '(bool-type)
  '(proc-type type? type?)
  '(var-type number?))

(define-datatype 'prg
  '(exp-prg exp)
  '(dec-prg dec))

(define-datatype 'dec
  '(pat-dec pat? exp?)
  '(rec-dec symbol? type? match?))

(define-datatype 'exp
  '(const-int-exp integer?)
  '(id-exp symbol?)
  '(app-exp exp? exp?)
  '(add-exp exp? exp?)
  '(diff-exp exp? exp?)
  '(mul-exp exp? exp?)
  '(div-exp exp? exp?)
  '(equal-exp exp? exp?)
  '(let-exp dec? exp?)
  '(if-exp exp? exp? exp?)
  '(fn-exp match?))

(define-datatype 'match
  '(pat-match pat? exp?))

(define-datatype 'pat
  '(apat-pat apat? type?))

(define-datatype 'apat
  '(id-apat symbol?))

;; Recursive abstract syntax translation procedures
(define (sml->scheme prg)
  (match prg
	((exp-prg ?exp) (exp->scheme exp))
	((dec-prg ?dec) (dec->scheme dec))
	(? (error "sml->scheme: unkown program statement" prg))))

(define (dec->scheme dec)
  (match dec 
	((pat-dec (apat-pat (id-apat ?s)) ?e) `(define ,s ,(exp->scheme e)))
	((rec-dec ?s ?e) `(define ,s ,(exp->scheme e)))
	(? (error "dec->scheme: unkown declaration" dec))))

(define (exp->scheme exp)
  (display "exp: ") (display exp) (newline)
  (match exp
	((const-int-exp ?i) i)
	((id-exp ?s) s)
	((app-exp ?e1 ?e2) `(,(exp->scheme e1) ,(exp->scheme e2)))
	((add-exp ?e1 ?e2) `(+ ,(exp->scheme e1) ,(exp->scheme e2)))
	((diff-exp ?e1 ?e2) `(- ,(exp->scheme e1) ,(exp->scheme e2)))
	((mul-exp ?e1 ?e2) `(* ,(exp->scheme e1) ,(exp->scheme e2)))
	((div-exp ?e1 ?e2) `(/ ,(exp->scheme e1) ,(exp->scheme e2)))
	((equal-exp ?e1 ?e2) `(equal? ,(exp->scheme e1) ,(exp->scheme e2)))
	((let-exp ?d ?e) (let->scheme d e))
	((if-exp ?e1 ?e2 ?e3) `(if ,(exp->scheme e1) ,(exp->scheme e2) ,(exp->scheme e3)))
	((fn-exp ?m) (fn->scheme m))
	(? (error "exp->scheme: unknown exp" exp))))

;; Translation procedures that need special handling
(define (let->scheme dec exp)
  (match dec
	((pat-dec (apat-pat (id-apat ?s)) ?e) `(let ((,s ,(exp->scheme e))) ,(exp->scheme exp)))
	(((rec-dec ?s ?e) `(let ((,s)) ,(exp->schem exp))))
	(? (error "let->scheme: unknown dec" dec))))

(define (fn->scheme m)
  (match m
	((pat-match (apat-pat (id-apat ?s)) ?e) `(lambda (,s) ,(exp->scheme e)))
	(? (error "fn->scheme: unkown match" m))))

;; Type inference
(define-datatype 'type-env
  '(empty-type-env)
  ; extend-type-env var val env
  '(extend-type-env symbol? type? type-env?))

(define (apply-type-env env search-var)
  (match env
	((empty-type-env) (error "apply-type-env: can not find type for var: " search-var))
	((extend-type-env ?saved-var ?saved-type ?saved-env)
	 (if (eqv? saved-var search-var)
		 saved-type
		 (apply-type-env saved-env search-var)))
	(? (error "apply-type-env: bad env" env))))

(define (init-type-env) '())

(define (check-equal-type ty1 ty2 exp)
  (if (not (equal? ty1 ty2))
	  (report-unequal-types ty1 ty2 exp)))

(define (report-unequal-types ty1 ty2 exp)
  (display
   (list "check-equal-type: types does not match " ty1 ty2 exp))
  (newline))

(define (type-to-external-form type)
  (match type
	((int-type) 'int)
	((bool-type) 'bool)
	((proc-type ?arg-type ?result-type)
	 (list (type-to-external-form arg-type) "->" (type-to-external-form result-type)))
	(? (error "type-to-external-form: unkown type: " type))))

(define (type-of exp)
  (match exp
	((const-int-exp ?) (int-type))
	(? (error "type-of: unkown expression" exp))))

;; Repl
(define (print-sml exp) (display exp) (newline))
(define (print-type type) (display (type-to-external-form type)) (newline))

(define type-env (init-type-env))

(define (repl-sml)
  (display '>)
  (let ((statement (read-sml)))
	(display statement) (newline)
	(let ((type (type-of statement type-env)))
	  (match type
		((none) (display 'typing-failure) (newline))
		((some ?t)
		 (print-type t)
		 (let ((translated (sml->scheme statement)))
		   (display translated) (newline)
		   (let ((result (eval translated environment)))
			 (print-sml result)))))
	  (repl-sml))))
	
