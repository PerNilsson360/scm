;; Introduction rule for conjuction
;; p : A  q : B
;;-------------
;; (p, q) : A /\ B

;; Elimination rule for conjuction
;; r : (A /\ B)
;; fst r : A
;; 
;; r : (A /\ B)
;; snd r : B

;; Computation 
;; fst r -> p
;; snd r -> q

(define (mk-prop-var name type value) (list name type value))
(define (prop-var-name p) (car p))
(define (prop-var-type p) (cadr p))
(define (prop-var-value p) (caddr p))

(define (conj-intro p q) (list 'conj p q)
(define (elim-conj-left r) (cadr r))
(define (elim-conj-right r) (caddr r))


;; Introduction rule for implication
;; 
;;            [x:A]
;;              .
;;              .
;;             e:B
;;          --------
;;      (lambda ((x A) (e B)) : A --> B

;; Elimination rule for implication
;;       q: A --> B  a: A
;;       ----------------
;;       (q a) : B


;; prove given  a: A --> B and b:B --> C  A --> C holds


;;  [x: A]1   a : A --> B
;;  ----------------
;;    (a x) : B           b : B --> C
;; -----------------------------------
;;        (b (a x)) : C
;; ------------------------ 1
;;  (lambda (x:A) (b (a x))) : A --> C
;;------------------------------------ abstract over a and b
;; (lambda ((a : A -> B) (b : B -> C) (x : A)) (b (a x)))


(define x 'A)
(define (a x) (if (eq? x 'A) 'B (error "a: argument has wrong type")))
(define (b x) (if (eq? x 'B) 'C (error "a: argument has wrong type")))
(define prof (lambda (a b x) (b (a x))))


