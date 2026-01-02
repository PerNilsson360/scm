;; simple unit test framework
(define (ut-make-suit) '())

(define (ut-add-test suite name body)
  (cons '(,name (lambda (),@body))) suite)
