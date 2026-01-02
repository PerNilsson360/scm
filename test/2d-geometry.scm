(load "/home/per-nilsson/git/scm/src/srfi9-records.scm")

(define (print-line s) (display s) (newline))

(define (max a b ) (if (> a b) a b))
(define (min a b ) (if (< a b) a b))

(define-record-type '<point-2d> '(<point-2d> x y))
(define-record-type '<vector-2d> '(<vector-2d> x y))
(define-record-type '<line-2d>  '(<line-2d> p0 p1))

(define (line-2d-max-x l) (max (<point-2d>:x (<line-2d>:p0 l)) (<point-2d>:x (<line-2d>:p1 l))))
(define (line-2d-min-x l) (min (<point-2d>:x (<line-2d>:p0 l)) (<point-2d>:x (<line-2d>:p1 l))))
(define (line-2d-max-y l) (max (<point-2d>:y (<line-2d>:p0 l)) (<point-2d>:y (<line-2d>:p1 l))))
(define (line-2d-min-y l) (min (<point-2d>:y (<line-2d>:p0 l)) (<point-2d>:y (<line-2d>:p1 l))))
  

(define (point-2ds->vector p1 p2)
  (let ((v (<vector-2d> (- (<point-2d>:x p2) (<point-2d>:x p1))
						(- (<point-2d>:y p2) (<point-2d>:y p1)))))
	(vector-2d-scalar-mul v (/ 1 (vector-2d-length v)))))

(define (vector-2d-add u v)
  (+ (* (<vector-2d>>:x u) (<vector-2d>:x v))
	 (* (<vector-2d>>:y u) (<vector-2d>:y v))))

(define (vector-2d-scalar-mul v s)
  (<vector-2d> (* s (<vector-2d>:x v)) (* s (<vector-2d>:y v))))
	 
(define (vector-2d-length v)
  (sqrt (+ (expt (<vector-2d>:x v) 2) (expt (<vector-2d>:y v) 2))))


;; Intersection calculation usings cramers rule.
;; a1x + b1y = c1
;; a2x + b2y = c2

;; D =  |a1, b1|
;;      |a2, b2|  

;; Dx =  |c1, b1|
;;       |c2, b2|  

;; Dx =  |a1, c1|
;;       |a2, c2|  

;;  x = Dx/D, 
;;  y = Dy/D

;;  Lines are represented as
 
;;  P(a) = (1 - a) P0 + aP1
;;  Q(b) = (1 - b) Q0 + bQ1

;;  linear equation system with a,b as unkowns
 
;;  a(xp1 - xp0) + b(xq0 - xq1) = xq0 -xp0
;;  a(yp1 - yp0) + b(yq0 - yq1) = yq0 - yp0

;;  ax1 + bx2 = c1
;;  ay1 + by2 = c2

;;  Da = |c1 x2|
;;       |c2 y2|

;;  Db = |x1 c1|
;;       |y1 c2|
(define (line-2d-intersect l0 l1)
  (let ((p0 (<line-2d>:p0 l0))
		(p1 (<line-2d>:p1 l0))
		(q0 (<line-2d>:p0 l1))
		(q1 (<line-2d>:p1 l1)))
	(let ((x1 (- (<point-2d>:x p1) (<point-2d>:x p0)))
		  (x2 (- (<point-2d>:x q0) (<point-2d>:x q1)))
		  (y1 (- (<point-2d>:y p1) (<point-2d>:y p0)))
		  (y2 (- (<point-2d>:y q0) (<point-2d>:y q1)))
		  (c1 (- (<point-2d>:x q0) (<point-2d>:x p0)))
		  (c2 ( -(<point-2d>:y q0) (<point-2d>:y p0))))
	  (let ((d (- (* x1 y2) (* y1 x2))))
		(if (= d 0)
			#f
			(let* ((da  (- (* c1 y2) (* c2 x2)))
				   (a (/ da d))
				   (db  (- (* x1 c2) (* y1 c1)))
				   (b (/ db d)))
			  (if (or (< a 0) (> a 1) (< b 0) (> b 1))
				  #f
				  (let ((x (+ (* (- 1 a) (<point-2d>:x p0))  (* a (<point-2d>:x p1))))
						(y (+ (* (- 1 a) (<point-2d>:y p0))  (* a (<point-2d>:y p1)))))
				  (<point-2d> x y)))))))))

;; Checks to see if a line is inside a bounding box
(define (line-2d-inside-box l0 x y width height)
  (let ((max-x (+ x width))
		(max-y (+ y height)))
	(if (or (< (line-2d-max-x l0) x)
			(< (line-2d-max-y l0) y)
			(> (line-2d-min-x l0) max-x)
			(> (line-2d-min-y l0) max-y))
		#f
		(let ((lines
			   (list (<line-2d> (<point-2d> x y) (<point-2d> max-x y))
					 (<line-2d> (<point-2d> x max-y) (<point-2d> max-x max-y))
					 (<line-2d> (<point-2d> x y) (<point-2d> x max-y))
					 (<line-2d> (<point-2d> max-x y) (<point-2d> max-x max-y)))))
		  (find (lambda (l1) (line-2d-intersect l0 l1)) lines)))))

(line-2d-intersect
 (<line-2d> (<point-2d> 0 0) (<point-2d> 0 640))
 (<line-2d> (<point-2d> 164 320) (<point-2d> (+ 164 20) 320)))

;; given a point p and a vector v move the point using parameter a
;; aka line on a parameteric form
(define (move-point-2d p v a)
  (let ((x (+ (<point-2d>:x p) (* a (<vector-2d>:x v)))))
	(let ((y (+ (<point-2d>:y p) (* a (<vector-2d>:y v)))))
	  (<point-2d> x y))))

;; Given 2 vectors calculate the angle between them
;; u * v = |u||v| cos a
;; a = acos (u * v / |u||v|
;(define (vector-2d-angle u v)

(define (gr-move-to-point-2d! v) (gr-move-to! (<point-2d>:x v) (<point-2d>:y v)))

