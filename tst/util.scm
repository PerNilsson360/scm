;; common utilities

(define (assert condition . args) (if (not condition) (error "ASSERT:" args)))
(define (println . args) (display args) (newline))
(define (tagged-list? exp tag) (and (pair? exp) (eq? (car exp) tag)))

(define (zip k l)
  (assert (= (length k) (length l)) 
	     "ZIP: lists has to be of same length"
	     k
	     l)
  (if (null? k)
      '()
      (cons (cons (car k) (car l)) (zip (cdr k) (cdr l)))))

;; Algebraic data type Some/None

(define (mk-some v) (list 'some v))
(define (some? d) (tagged-list? d 'some))
(define (some-val d) (cadr d))
(define (mk-none) (list 'none))
(define (none? d) (tagged-list? d 'none))
