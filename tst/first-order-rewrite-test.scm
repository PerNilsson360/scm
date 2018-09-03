(define z)
(define (s n))
(define (plus n m))
(define (plus z (s n)) (s n))
(define (plus (s n) (s m)) (s (plus n (s m))))
(plus (s z) (s z))
(plus n (s z))
(plus (s z) m)

(plus (s (s z)) (s (s z)))
(plus (s (s n)) (s (s z)))
(plus (s (s z)) (s (s m)))
(plus (s (s n)) (s (s m)))

