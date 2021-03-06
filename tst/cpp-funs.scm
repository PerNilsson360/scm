(define (initial-id-char? c) (or (char-alphabetic? c) (eq? c #\_))) 
(define (subsequent-id-char? c) 
  (or (char-alphabetic? c) (char-numeric? c) (eq? c #\_)))

(define (match-brace port)
  (let ((c (peek-char port)))
    (cond ((char-whitespace? c) (read-char port) (match-brace port))
	  ((eq? c #\() #t)
	  (else #f))))

(define (get-fun-name name names port)
  (display "baz") (newline)
  (let ((c (read-char port)))
    (display "baz") (newline)
    (cond ((and (char-whitespace? c) (match-brace port)) 
	   (parse port (cons (list->string (revert name)) names)))
	  ((eq? c #\() (parse port (cons (list->string (revert name)) names)))
	  ((subsequent-id-char? c) (get-fun-name (cons c name) names))
	  (else (parse port names)))))

(define (parse port names)
  (let ((c (read-char port)))
    (display c) (newline)
    (cond ((eof-object? c) names)
	  ((initial-id-char? c) (get-fun-name (list c) names port))
	  (else (parse port names)))))

(define (fun-names file)
  (let ((port (open-input-file file)))
    (display "foo") 
    (newline)
    (parse port '())))
