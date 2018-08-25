;; (load "unify-and-match.scm")
;; form term rewriting and all that

(load "util.scm")

(define (vname name num) (list name num))
;; constructor if num is not interesting
(define (vname0 name) (list name 0))
(define (vname-name v) (car v))
(define (vname-num v) (cadr v))

;; Algebraic data type for variables and terms, terms are function symbols
(define (vterm vname) (list 'vterm vname))
(define (vterm? v) (tagged-list? v 'vterm))
(define (vterm-vname v) (cadr v))

(define (term fname tlist) (list 'term fname tlist))
(define (term? t) (tagged-list? t 'term))
(define (term-fname t) (cadr t))
(define (term-tlist t) (caddr t))

;; Substitutions are association lists

(define (indom vname subst) (assoc vname subst))

;; application of substitution to a variable
(define (app-subst subst vname) (cadr (assoc vname subst)))

;; (define s '(((x 0) tx) ((y 0) ty)))


;; applies a substitution to a term
(define (lift subst t)
  (cond ((vterm? t)
	 (let ((vname (vterm-vname t)))
	   (if (indom vname subst)
	       (app-subst subst vname)
	       t)))
	((term? t)
	 (term (term-fname t) (map (lift subst) (term-list t))))
	(else (error "LIFT: wrong term type" t))))


;; matching		       
(define (pattern-match pattern obj)
  (define (inner equations subst)
    (if (null? equations)
	(mk-some subst)
	(let* ((equation (car equations))
	       (pattern (car equation))
	       (obj (cdr equation)))
	  (cond ((vterm? pattern)
		 (let ((var-name (vname-name (vterm-vname pattern))))
		   (if (indom var-name subst)
		       (if (equal? (app-subst subst var-name) obj)
			   (inner (cdr equations) subst)
			   (mk-none))
		       (inner (cdr equations)
			      (cons (cons var-name obj) subst)))))
		((vterm? obj) (mk-none))
		((and (term? pattern) (term? obj))
		 (let ((f (term-fname pattern))
		       (g (term-fname obj))
		       (patt-terms (term-tlist pattern))
		       (obj-terms (term-tlist obj)))
		   (if (equal? f g)
		       (inner (append (zip patt-terms obj-terms)
				      (cdr equations))
			      subst)
		       (mk-none))))
		(else (error "MATCH: wrong type in inner" pattern obj))))))
  (inner (list (cons pattern obj)) '()))

(define o1 (term 'f (list (vterm (vname0 'a)))))
(define o2 (term 'g (list (vterm (vname0 'a)))))
(define p1 (term 'f (list (vterm (vname0 'x)))))

;; Single step reduction
(define (rewrite rules t)
  (if (null? rules)
      (mk-none)
      (let* ((rule (car rules))
	     (l (car rule))
	     (r (cdr rule))
	     (subst (pattern-match (car rule) t)))
	(if (some? subst)
	    (mk-some (lift (some-val subst) r))
	    (rewrite (cdr rules)2 t)))))

(define (normalize rules t)
  (cond ((vterm? t) t)
	((term? t)
	 (let* ((u (term (term-fname t)
			 (map (normalize rules) (term-tlist t))))
		(v ((rewrite rules u))))
	   (if (none? v)
	       u			; u is in normal form
	       (normalize rules u))))
	(else (error "NORMALIZE: wrong term type" t))))
		
;; ((plus z (s n)) (s n))
;; ((plus (s n) (s m)) (s (plus n (s m))))
