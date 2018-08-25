;; makes a set containing one symbol
(define (make-set symbol) (list symbol))

;; add a symbol to a set 
(define (set-add symbol set)
  (if (memv symbol set)
      (error "SET_ADD: symbol allready in the set")
      (cons symbol set)))

;; example of a grammar
;; grammar's are lists of productions
(define g '((S (seq A B C d))
	    (A (alt e f epsilon))
	    (B (alt g h epsilon))
	    (C (alt p q))))

(define (first-production grammar) (car grammar))  
(define (rest-production grammar) (cdr grammar))
(define (no-more-productions? grammar) (null? grammar))
(define (left-hand-side production) (car production)) 
(define (right-hand-side production) (cadr production)) 
(define (concatenation? right-hand-side) (eq? (car right-hand-side) 'seq))
(define (alternation? right-hand-side) (eq? (car right-hand-side) 'alt))
(define (empty? symbol) (eq? symbol 'epsilon))
(define (find-left-hand grammar symbol) (assoc symbol grammar))

(define (first grammar left-hand-side)
  (let ((

(define (first-single-symbol grammar)
  (let 
