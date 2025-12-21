#!/home/per-nilsson/git/scm/scheme

(if (not (= (length command-line) 3))
    (begin (display "usage: mk-prelude-file <prelude>")
	   (newline)
	   (quit)))

(define (inc i) (+ i 1))

(define (write-string s port)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (if (< i len)
	  (begin (write-char (string-ref s i) port)
		 (loop (inc i)))))))

(define (skip-char? c previous)
  (or (and (equal? c #\space) (equal? previous #\space))
      (equal? c #\cr)
      (equal? c #\lf)))
	  
(define (dump-prelude in-port out-port)
  (let loop ((c (read-char in-port))
	     (previous-char #\z))	; dummy
    (if (not(eof-object? c))
	(begin (if (not (skip-char? c previous-char))
		   (write-char c out-port))
	       (loop (read-char in-port) c)))))
    
(define in-file-name  (list-ref command-line 2))
(define in-port (open-input-file in-file-name))
(define out-port (open-output-file "prelude.c"))

(write-string "/* Auto generated from prelude.scm using mk-prelude-file.scm */" out-port)
(write-char #\newline out-port)
(write-string "const char* prelude = R" out-port)
(write-char #\" out-port)
(write-char #\( out-port)
(dump-prelude in-port out-port)
(write-char #\) out-port)
(write-char #\" out-port)
(write-char #\; out-port)

(close-output-port out-port)
(close-input-port in-port)
