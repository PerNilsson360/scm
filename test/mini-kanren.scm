(define (fail x) '())
(define (succeed x) (list x))

(define (disj f1 f2) (lambda (x) (append (f1 x) (f2 x))))
(define (conj f1 f2) (lambda (x) (apply append (map f2 (f1 x)))))

(define (cout . args) (for-each display args))

(cout "test1"
      ((disj
	(disj fail succeed)
	(conj
	 (disj (lambda (x) (succeed (+ x 1)))
	       (lambda (x) (succeed (+ x 10))))
	 (disj succeed succeed)))
       100))

(define (var name) (vector name))
(define var? vector?)

(define empty-subst '())
(define (ext-s var value s) (cons (cons var value) s))

(define (lookup var s)
  (if (not (var? var)) 
      var
      (let ((association (assq var s)))
	(if (not association)
	    var
	    (lookup (cdr association) s)))))

(define (unify t1 t2 s)
  (let ((t1 (lookup t1 s))
	(t2 (lookup t2 s)))
    (cond ((eq? t1 t2) s)
	  ((var? t1) (ext-s t1 t2 s))
	  ((var? t2) (ext-s t2 t1 s))
	  ((and (pair? t1) (pair? t2))
	   (let ((s (unify (car t1) (car t2) s)))
	     (and s (unify (cdr t1) (cdr t2) s))))
	  ((equal? t1 t2) s)
	  (else #f))))

(define vx (var 'x))
(define vy (var 'y))
(define vz (var 'z))
(define vq (var 'q))

(unify vx vy empty-subst)
(unify vx 1 (unify vx vy empty-subst))
(lookup vy (unify vx 1 (unify vx vy empty-subst)))
(unify (cons vx vy) (cons vy 1) empty-subst)
(unify 1 1 empty-subst)

(define (== t1 t2)
  (lambda (s)
    (let ((u (unify t1 t2 s)))
      (if u
	  (succeed u)
	  (fail s)))))

(define (run g) (g empty-subst))

(define (choice var lst)
  (if (null? lst) 
      fail
      (disj (== var (car lst)) (choice var (cdr lst)))))

(run (choice 2 '(1 2 3)))
(run (choice 10 '(1 2 3)))
(run (choice vx '(1 2 3)))

(define (common-el l1 l2) (conj (choice vx l1) (choice vx l2)))

(run (common-el '(1 2 3 4) '(3 4 5)))

(define (conso a b l) (== (cons a b) l))

(run (conso 1 '(2 3) vx))
(run (conso vx vy (list 1 2 3)))