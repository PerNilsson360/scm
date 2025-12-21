; selfevaluating
; variable
; quoted
; assignment
; definition
; if
; lambda
; begin

(define (inc x) (+ x 1))

(define (extend-context vars context) (cons vars context))
(define (add-var-to-first-frame var context) 
  (if (not (null? context)) 
      (cons (cons var (car context)) (cdr context))))
(define (get-var-index var vars index)
  (if (null? vars)
      -1
      (if (and (symbol? vars) (eq? var vars))
	  index
	  (if (eq? var (car vars))
	      index
	      (get-var-index var (cdr vars) (inc index))))))

;; if a variable is bound by a lambda or similar 
;; variable is replaced by '(var frame-index var-index)
;; if it is not bound the var is not translated
(define (var->var-info var context frame-index)
  (if (null? context)
      var
      (let ((var-index (get-var-index var (car context) 0)))
	(if (= var-index -1)
	    (var->var-info var (cdr context) (inc frame-index))
	    (list '--scm-name-free-var-- var frame-index var-index)))))	    
(define (make-empty-context) '())


(define (mk-lambda vars rest) (cons 'lambda (cons vars rest)))

(define (exp->name-free exp context)
  (match exp 
	 ('() '())
	 ((lambda ?vars . ?rest)
	  (if (> (length vars) 0)
	      (mk-lambda vars (exp->name-free rest (extend-context vars context)))
	      (mk-lambda vars (exp->name-free rest context))))
	 ((?hd . ?tl) (cons (exp->name-free hd context) (exp->name-free tl context)))
	 (?object (if (symbol? object)
		      (var->var-info object context 0)
		      object))))
  
(exp->name-free '(lambda (x y) (if (< x y) (+ 1 2 x) ((lambda (x) (+ x y)) 1))) '() -1)


(define (foo a b) 
  (define a 1)
  (display a))
