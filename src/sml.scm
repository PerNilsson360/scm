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

(define-datatype 'type-result
  '(error-type-result string?)
  ;; success-type-result type substitution list
  '(success-type-result type? list?))

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

;; Substitutions
(define (empty-subst) '())

;; substitute tv for t1 in t0, t0[tv = t1]
;; Type x Tvar x Type -> Type
(define (apply-one-subst ty0 tvar ty1)
  (match ty0
	((int-type) (int-type))
	((bool-type) (bool-type))
	((proc-type ?arg-type ?result-type)
	 (proc-type (apply-one-subst arg-type tvar ty1)
				(apply-one-subst result-type tvar ty1)))
	((var-type ?sn) (if (equal? ty0 tvar) ty1 ty0))
	(? (error "apply-one-subst: unkown type" ty0))))
  
;; Type x Subst -> Type
(define (apply-subst-to-type ty subst)
  (match ty
	((int-type) (int-type))
	((bool-type) (bool-type))
	((proc-type ?arg-type ?result-type)
	 (proc-type (apply-subst-to-type arg-type subst)
				(apply-subst-to-type result-type subst)))
	((var-type ?sn)
	 (let ((s (assoc ty subst))) (if s (cdr s) ty)))))

;; Subst x Tvar x Type -> Subst
(define (extend-subst subst tvar ty)
  (cons
   (cons tvar ty)						; add the new substitution
   (map (lambda (p)                     ; apply the the substitution
		  (let ((old-lhs (car p))		; on the allready added ones
				(old-rhs (cdr p)))
			(cons old-lhs (apply-one-subst old-rhs tvar ty))))
		subst)))

;; Type x Type x Subst x Exp -> Subst
(define (unifier ty1 ty2 subst exp)
  (let ((ty1 (apply-subst-to-type ty1 subst))
		(ty2 (apply-subst-to-type ty2 subst)))
	(cond ((equal? ty1 ty2) subst)
		  ((var-type? ty1)
		   (if (no-occurence? ty1 ty2)
			   (extend-subst subst ty1 ty2)
			   (error "unifier: failed occuence check" ty1 ty2 exp)))
		  ((var-type? ty2)
		   (if (no-occurence? ty2 ty1)
			   (extend-subst subst ty2 ty1)
			   (error "unifier: failed occuence check" ty2 ty1 exp)))
		  ((and (proc-type? ty1) (proc-type? ty2))
		   (let ((subst (unifier (proc-type->arg-type ty1)
								 (proc-type->arg-type ty2)
								 subst
								 exp)))
			 (let ((subst (unifier (proc-type->result-type ty1)
								   (proc-type->result-type ty2)
								   subst
								   exp)))
			   subst)))
		  (else (error "unifier: failed unify" ty1 ty2 exp)))))

;; Tvar x Type -> Bool
(define (no-occurence? tvar ty)
  (match ty
	((int-type) #t)
	((bool-type) #t)
	((proc-type ?arg-type ?result-type)
	 (and  (no-occurence? tvar arg-type) (no-occurence? tvar result-type)))
	((var-type ?) (not (equal? tvar ty)))
	(? (error "no-occurence?: not supported type" ty))))

(define (type-of-prg exp env subst)
  (call-with-current-continuation
   (lambda (throw)
	 (match exp
	   ((exp-prg ?exp) (type-of exp env subst throw))
	   ((dec-prg ?dec) (type-of dec env subst throw))
	   (? (throw (error-type-result "type-of-prg: unkown prg")))))))

(define (type-of-numeric-operator left right env subt throw)
  ;; TODO
  )

(define (type-of exp env subst throw)
  (match exp
	((const-int-exp ?) (success-type-result (int-type) subst))
	(? (throw (error-type-result "type-of: unkown expression")))))

;; Repl
(define (print-sml exp type)
  (display exp)
  (display #\space)
  (display #\:)
  (display #\space)
  (display (type-to-external-form type))
  (newline))

(define (print-type type) (display (type-to-external-form type)) (newline))

(define type-env (init-type-env))

(define (repl-sml)
  (display 'sml>)
  (let ((statement (read-sml)))
	(display statement) (newline)
	(let ((result (type-of-prg statement type-env (empty-subst))))
	  (match result
		((error-type-result ?e) (display e) (newline))
		((success-type-result ?type ?subst)
		 (let ((translated (sml->scheme statement)))
		   (display translated) (newline)
		   (let ((result (eval translated environment)))
			 (print-sml result type))))
		(? (error "repl-sml: unkown result of type checking")))
	    (repl-sml))))

	
