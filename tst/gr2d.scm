(load "matrix.scm")

(define gr2d-set-identity '())
(define gr2d-translate ())
(define gr2-rotate ())
(define gr2d-fill-polygon ())


(let ((m (make-matrix #(1 0 0) #(0 1 0) #(0 1 0))))
  (set! gr2d-set-identity
        (lambda () (set! m make-matrix #(1 0 0) #(0 1 0) #(0 1 0))))
  (set!
