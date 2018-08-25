((forall (A set) (P ((list A) --> Prop)))
 (P nil) -> 
 ((forall (a A) (l (list A)))(P l) -> (P (cons A a l))) ->
 ((forall (l (list A))) (P l)))

((forall v (l list)) 
 (P '())
 (--> (P l) (P (cons v l)))
 ((forall (l list)) (P l)))
      
(define (length l)
  (match l
	 (() 0)
	 ((?v . ?l) (+ 1 (length l)))))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

;; (forall ((list? l1) (list? l2)) (equal? (length (append l1 l2)) (+ (length l1) (length l2))))
;; basecase (eq? l1 '())
;; (equal? (length (append '() l2)) (+ (length '()) (length l2)))
;; (equal? (length  l2) (+ 0 (length l2)))
;; (equal? (length  l2) (length l2))
;; 
;; (equal? (length (append (cons 'x l1) l2)) (+ (length (cons 'x l1)) (length l2)))

(define (length-cons-null-is-one)
  (eq? 1 (length (cons 'x '()))))

(eq? 1  (if (null? (cons 'x '()))
	    0
	    (+ 1 (length (cdr (cons 'x '())))))))

(eq? 1 (let ((l (cons 'x '())))
	 (+ 1 (length (cdr l)))))

;; (eq? 1 (+ 1 (length (cdr (cons 'x '())))))
;; (eq? 1 (+ 1 (length '())))
;; (eq? 1 (+ 1 0))
;; #t

(define (list-length-property l1 v)
  (eq? (length (cons v l1)) 		
       (+ 1 (length l1))))

(define (list-length-property-base-case)
  (list-length-property '() 'v))

;  
; (+ 1 (length (cdr (cons v (cons x l1)))))
; (+ 1 (length (cons x l1)))


(define (foo) 1)
(define (forever) (begin (display "hej")(newline)(forever)))
