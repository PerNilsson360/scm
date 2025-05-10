;; (load "recursive-nat.scm")
(define (add1 n) (list 's n))

(define (which-nat n base step)
  (match n
    (z base)
    ((s ?n) (step n))
    (else (error "which-nat unkown case: " n))))

(define (step n) #f)

(define (z? n) (which-nat n #t step))
(z? 'z)
(z? '(s z))

(define (iter-nat n base step)
  (match n
    (z base)
    ((s ?n) (step (iter-nat n base step)))))

(define (step-plus n) (list 's n))
(define (plus n m) (iter-nat n m step-plus))
(plus 'z '(s z))
(plus '(s (s z)) '(s z))

(define (rec-nat n base step)
  (match n
    (z base)
    ((s ?n)
     (step n (rec-nat n base step)))))

(define (rec-step-plus n n-1) (list 's n-1))
(define (plus n m) (rec-nat n m rec-step-plus))
     
(plus 'z '(s z))
(plus '(s z) 'z)
(plus '(s (s z)) '(s z))

(define (make-step-mul m) (lambda (n-1 mul-n-1) (plus m mul-n-1)))
(define (mul n m) (rec-nat n 'z (make-step-mul m)))

(mul 'z '(s z))
(mul '(s z) 'z)
(mul '(s (s z)) '(s (s z)))


;; Converting peano numbers to/from scheme numbers
(define (nat->number-step n n-1) (+ 1 n-1))
(define (nat->number n) (rec-nat n 0 nat->number-step))
(nat->number '(s (s (s (s z)))))

(define (rec-number n base step)
  (match n
    (0 base)
    (?n (step n (let ((n-1 (- n 1))) n-1 (rec-number n-1 base step))))))

(define (number->nat-step n n-1) (list 's n-1))
(define (number->nat n) (rec-number n 'z number->nat-step))

;; scheme list processing with rec-nat
(define (rec-list l base step)
  (match l
    ('() base)
    ((?e . ?es) (step e es (rec-list es base step)))))

(define (list-length-step e es res) (add1 res))
(define (list-length l) (rec-list l 0 list-length-step))

(list-length '(1 2 3 4))

(define (list-append-step e es res) (cons e res))
(define (list-append l1 l2) (rec-list l1 l2 list-append-step))
(list-append '(1 2 3) '(4 5 6))

(define (snoc e l) (rec-list l (list e) list-append-step))

(snoc 1 '(1 2 3 4))

(define (list-reverse-step e es res) (snoc e res))
(define (list-reverse l) (rec-list l '() list-reverse-step))
(list-reverse '(1 2 3 4))

;; how to implement take drop seems to need "if" and the length of the list


;; tree (value left right)
(define (rec-tree t base step)
  (match t
    ('() base)
    ((?v ?l ?r) (step v (rec-tree l base step) (rec-tree r base step)))))

(define (step
  


