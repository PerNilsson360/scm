(define (add key-val-pair env) (cons key-val-pair env))
(define (find key env) (assq key env))

(define (extend-if-consistent key val env)
  (let ((env-entry (find key env))
	(entry (cons key val)))
    (cond ((not env-entry) (add (cons key val) env))
	  ((equal? env-entry entry) env)
	  (else throw 'pattern-match-failure))))

(define (pattern-var? variable) 
  (and (symbol? variable) (eq? (string-ref (symbol->string variable) 0) #\?)))

(define (name-from-pattern-var v) 
  (let ((s (symbol->string v)))
    (string->symbol (substring s 1 (string-length s)))))

(define (dont-care-pattern? v) (eq? v '?))

(define (pattern-match data pattern env)
  (cond ((pattern-var? pattern) 
	 (if (dont-care-pattern? pattern)
	     env
	     (extend-if-consistent (name-from-pattern-var pattern)
				   data
				   env)))
	 ((and (pair? data) (pair? pattern))
	  (let ((new-env (pattern-match (car data) (car pattern) env)))
	    (pattern-match (cdr data) (cdr pattern) new-env)))
	 ((eq? data pattern) env)
	 (else (throw 'pattern-match-failure))))


(define (foo x)
  (match x
	 ("t" 'true)
	 ("f" 'false)
	 (? 'darn)))


(define (foo x)
  (match x
	 ((garb ?x ?y) (+ x y))
	 (? 'darn)))


(define (foo x) (+ 1 x))

(match 1 ("t" 'true) (? 'darn))

(match 1 (?x (+ 1 x)))

(define (mk-nil) 'nil)
(define (mk-cons head tail) (list 'cons head tail))

(define (head l) 
  (match l
	 (nil 'nil)
	 ((cons ?head ?tail) head)))


(define-type 
  'list
  '(nil)
  '(cons hd tl))

(define (inner name variant)
  (let ((name (string->symbol (string-append "dt-" 
					     (symbol->string name)
					     "-"
					     (symbol->string (car variant)))))
	(args (cdr variant)))
    (eval `(define (,name ,@args)
	     (list (quote ,name) ,@args))
	  environment)))

(define (define-type name variant . variants)
  (define (inner variant)
    (let ((name (string->symbol (string-append "dt-" 
					       (symbol->string name)
					       "-"
					       (symbol->string (car variant)))))
	  (args (cdr variant)))
      (eval `(define (,name ,@args)
	       (list (quote ,name) ,@args))
	    environment)))
  (inner variant)
  (for-each inner variants))

(define (Zero) 'O)
(define (Succ a) `(Succ ,a))

(define (add a b)
  (match a
	 (O b)
	 ((Succ ?a) (add a (Succ b)))))


(type ((Bool Set) 
       (true Bool) 
       (false Bool)))

(type ((Nat Set) 
       (zero Nat) 
       ((succ (a Nat)) Nat)))

(type (((List (A Set)) Set)
       (nil (List A))
       ((cons (a A) 
	      (l (List A))) 
	(List A))))

(type ((Vec (A Set) (n Nat))
       (nil (Vec A zero))
       ((cons (a A)
	      (v (Vec A n)))
	(Vec A (succ n)))))

(type ((Prod Set) 
       (A Set) 
       (B Set)
       ((prod (* A B))
	(a A) 
	(b B))))


(define ((not Bool) (a Bool)) 
  (match a 
	 (true (false))
	 (false (true))))

(define ((id A) (A Set) (x A)) x)
(define ((k A) (A Set) (B Set) (x A) (y B)) x)
(define ((s C) (A Set) (B Set) (C set) 
	 ((f C) (a A) (b B))
	 ((g B) (a A))
	 (x A))
  (f (g x)))

(define ((map (List B)) (A set) (B Set) 
	 ((f B) (a A))
	 (l (List A)))
  (match l
	 (nil nil)
	 ((cons ?x ?xs) (cons (f x) (map A B f xs)))))

(define ((fst A) (A Set) (B Set) (a (Prod A B)))
  (match a
	 ((prod ?a ?b) a)))
