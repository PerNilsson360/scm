;; add parsing of []
;; add matching on schemetypes
;; add tuples 

(define int
  ([O]
    [succ int]))

(: plus (lambda (int int) int))
(define plus
  (lambda (a b)
    (match a
	   ([O] b)
	   ([succ c] (plus c [succ b])))))
