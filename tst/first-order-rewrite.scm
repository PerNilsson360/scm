;; First order rewriting

(load "util.scm")

(define global-context '())

(define (add-rule rule)
  (set! global-context (cons rule global-context)))

(define (define? exp) (tagged-list? exp 'define))
(define (variable? exp) (symbol? exp))
(define (term? exp) (pair? exp))

(define (eval exp)
  (cond ((define? exp) (eval-def exp global-context))
	((term? exp) (normalize exp global-context))
	(else (error "EVAL: unkown expression" exp))))

(define (fun-app? exp) (pair? exp))
(define (fun-symbol exp) (car exp))
(define (fun-args exp) (cdr args))

(define (normalize exp context)
  ((cond ((variable? exp) exp)
	 ((fun-app? exp) )
	 (else "NORMALIZE: wrong type in normalize" exp))))

(define (display-prompt) (display '>))

(define (read-eval-print)
  (display-prompt)
  (let* ((input (read))
	 (output (eval input)))
    (display output)
    (newline)
    (read-eval-print)))


;; ((plus z (s n)) (s n))
;; ((plus (s n) (s m)) (s (plus n (s m))))
