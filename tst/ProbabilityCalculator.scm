(define (sum list)
  (if (null? list)
      0
      (+ (car list) (sum (cdr list)))))

(define (load-prob list)
  (define (capacity-adjusted-load capacity-load)
    (let ((capacity (car capacity-load))
	  (load (cadr capacity-load)))
      (* capacity (-  1 load))))
  (define (inner list sum)
    (if (null? list)
	'()
	(cons (/ (capacity-adjusted-load (car list)) sum) (inner (cdr list) sum))))
  (let ((s (sum (map capacity-adjusted-load list))))
    (inner list s)))

 (load-prob '((3 0) (3 0) (2 0) (2 0.0)))
 (load-prob '((3 0.5) (3 0.6) (2 0.5) (2 0.4)))

(define (zero-load li)
  (map
   (lambda (x) (list (car x) 0))
   li))

(define (load-adjusted-prob capacity-pair load-pair)
  (/ (car capacity-pair) (cadr load-pair)))

(define (load-prob2 li)
  (let* ((capacity-probs (load-prob (zero-load li)))
	 (s (sum (map load-adjusted-prob capacity-probs li))))
    s))