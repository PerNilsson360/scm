(define (inc x) (+ x 1))

(define (make-matrix rows columns . value)
  (if (> (length value) 1)
      (error "MAKE_MATRIX: wrong # of arguments"))
  (let ((v (if (null? value) value (car value))))
    (let loop ((m (make-vector rows))
               (i 0))
      (if (< i rows)
          (begin
            (vector-set! m i (make-vector columns v))
            (loop m (inc i)))
          m))))

(define (matrix vec . vecs)
  (let* ((row-length (inc (length vecs)))
        (column-length (vector-length vec))
        (m (make-matrix row-length column-length)))
    (let loop ((vs (cons vec vecs))
               (i 0))
      (cond ((null? vs) m)
            ((not (= (vector-length (car vs)) column-length))
             (error "MATRIX: inconsistent column length"))
            (else (vector-set! m i (car vs))
                  (loop (cdr vs) (inc i)))))))

(define (matrix-row-length m) (vector-length m))
(define (matrix-column-length m) (vector-length (vector-ref m 0)))
(define (matrix-ref m i j) (vector-ref (vector-ref m i) j))
(define (matrix-set! m i j v) (vector-set! (vector-ref m i) j v))


(define (matrix-mul a b)
  (let ((row-length (matrix-row-length a))
        (column-length (matrix-column-length b)))
    (if (not (= row-length column-length))
        (error "MATRIX-MUL: wrong dimensions in matrix multiplication"))
    (let ((m (make-matrix row-length column-length)))
      (let loop ((i 0)
                 (j 0)
                 (k 0)
                 (acc 0))       ;accumulated value
        (display (list i j k acc)) (newline)
        (if (< i row-length)
            (if (< j column-length)
                (if (< k row-length)
                    (loop i j (inc k) (+ acc (* (matrix-ref a i k) (matrix-ref b k j))))
                    (begin (matrix-set! m i j acc)
                           (loop i (inc j) 0 0)))
                (loop (inc i) 0 0 0))
            m)))))
            
            
            
            
    
