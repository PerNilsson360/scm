(define (mk-empty-type-context) '())
(define (add-type context type) (cons type context))
(define (add-types context vars types) (append (map list vars types) context))
(define (find-type context identifier) 
  (let ((result (assoc identifier context)))
    (match result
	   ((?id ?type) type)
	   (? '()))))

(define (type-failure exp target-type infered-type)
  (error "TYPE_CHECK: failure exp" exp 
	 "target-type" target-type
	 "infered-type" infered-type))

(define (type-check exp context)
  (match exp
	 ((define ?a (lambda ?vars ?body))
	    (let ((target-type (find-type context a)))
	      (match target-type
		     ((lambda ?var-types ?body-type)
		      (if (= (length vars) (length var-types))
			  (let ((infered-body-type 
				 (type-check body (add-types context vars var-types))))
			    (if (equal? infered-body-type body-type)
				target-type
				(type-failure exp target-type infered-body-type)))
			  (type-failure exp 'wrong-number-of-args '())))
		     (? (type-failure exp target-type '())))))
	 ((define ?a ?b) 
	  (let ((target-type (find-type context a))
		(infered-type (type-check b context)))
	    (if (equal? target-type infered-type)
		target-type
		(type-failur exp target-type infered-type))))
	 ((?rator . ?rands) 
	  (let ((rator-type (find-type context rator))
		(rand-types (map (lambda (x) (type-check x context)) rands)))
	    (match rator-type
		   ((lambda ?var-types ?body-type)
		    (if (equal? var-types rand-types)
			body-type
			(type-failure exp rator-type rand-types)))
		   (? (type-filure exp rator-type '())))))
	 (?x (cond ((number? x) 'number)
		   ((char? x) 'char)
		   ((boolean? x) 'boolean)
		   ((string? x) 'string)
		   ((vector? x) 'vector)
		   ((symbol? x) (find-type context x))
		   (else (type-failure exp 'bomb 'bomb))))))

;; (: a number)
;; (define a 1)
;; (: b (lambda (number) number))
;; (define b (lambda (x) 1))

(type-check '(define a 1) '((a number)))
(type-check '(define b (lambda (x) 1)) '((b (lambda (number) number))))
(type-check '(define b (lambda (x) 1)) '((b (lambda (number number) number))))
(type-check '(define b (lambda (x) 1)) '((b (lambda (number) char))))
(type-check '(b 1) '((b (lambda (number) number))))
(type-check '(b 1 2) '((b (lambda (number) number))))
(type
(type-check '(define b (lambda (x y) (+ x y))) 
	    '((b (lambda (number number) number)) (+ (lambda (number number) number))))



