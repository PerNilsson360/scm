;; type term = 
;; | TmTrue of info
;; | TmFalse of info
;; | TmIf of info * term * term * term
;; | TmZero of info
;; | TmSucc of info * term
;; | TmPred of info * term 
;; | TmIsZero of info * term

;; info is skipped

(define s '(tm-succ (tm-zero)))

(define (numerical-val? term)
  (match term
	 ((tm-zero) #t)
	 ((tm-succ ?s) (numerical-val? s))
	 (? #f)))

(define (val? term)
  (match term
	 ((tm-true) #t)
	 ((tm-false) #t)
	 (? (numerical-val? term))))

(define (eval-one term)
  (match term
	 ((tm-if (tm-true) ?t2 ?t3) t2)
	 ((tm-if (tm-false) ?t2 ?t3) t3)
	 ((tm-if ?t1 ?t2 ?t3) 
	  (let ((predicate (eval-one t1)))
	    `(tm-if ,predicate ,t2 ,t3)))
	 (? #f)))
