;; general
(define (inc a) (+ a 1))
(define (assert b message) (if (not b) (error message)))

;; matrix
(define (make-matrix rows columns)
  (let ((matrix (make-vector columns 0)))
    (define (inner index)
      (if (= index columns) 
	  matrix
	  (begin (vector-set! matrix  index (make-vector rows 0))
		 (inner (+ index 1)))))
    (inner 0)))

(define (matrix-set! matrix row column object) 
  (vector-set! (vector-ref matrix column) row object))

(define (matrix-ref matrix row column) 
  (vector-ref (vector-ref matrix column) row))

(define (matrix-row-size matrix) (vector-length (vector-ref matrix 0)))
(define (matrix-column-size matrix) (vector-length matrix))


(define (list-replace l old new) (map (lambda (x) (if (= x old) new x)) l))

;; changes all values matching old to new
;; in the matrix
(define (matrix-change-entry matrix old new)
  (let ((rows (matrix-row-size matrix))
	(cols (matrix-column-size matrix)))
    (let row-loop ((i 0))
      (if  (< i rows)
	   (let col-loop ((j 0))
	     (newline) (display i) (newline) (display j) (newline)
	     (if (< j cols)
		 (if (= (matrix-ref matrix i j) old)
		     (begin (matrix-set! matrix i j new)
			    (col-loop (inc j)))
		     (col-loop (inc j)))
		 (row-loop (inc i))))))))
	    	       
;; state machines 
;; columns are characters			
;; rows are states
(define (nfa-n-states nfa) (matrix-row-size nfa))

;; start and end state has always the same index
(define start-state-index 0)
(define end-state-index 1)
(define nfa-n-columns 256)		; one slot / char

(define (make-nfa char) 
  (let ((matrix (make-matrix 3 nfa-n-columns)))
    (matrix-set! matrix start-state-index 
		 (char->integer char) 
		 (list end-state-index))
    matrix))

(define (nfa-insert dest source dest-start-row)
  (define (column-loop d-row s-row column)
    (if (< column nfa-n-columns)
	(begin (matrix-set! dest 
			    d-row 
			    column 
			    (matrix-ref source s-row column))
	       (column-loop d-row s-row (inc column)))))
  (let ((s-rows (matrix-row-size source)))
    (let row-loop ((di dest-start-row)
		   (si 0))
      (if (< si s-rows)
	  (begin (column-loop di si 0)
		 (row-loop (inc di)(inc si)))))))

(define (nfa-concat nfa1 nfa2)
  (let* ((nfa1-n-states (nfa-n-states nfa1))
	 (size (+ nfa1-n-states (nfa-n-states nfa2)))
	 (matrix (make-matrix size nfa-n-columns 0)))
    (nfa-insert matrix nfa1 0)
    ; change the end state of the first nfa to point
    ; to begining of the second nfas start state
    (matrix-change-entry matrix end-state-index nfa1-n-states)
    (nfa-insert matrix nfa2 nfa1-n-states)
    matrix))

(define (nfa-alt nfa1 nfa2)

;; nfa -> dfa
;; SDFA = {}
;; Add #-Closure(s0) to SDFA as the start state
;; Set the only state in SDFA to “unmarked”
;; while SDFA contains an unmarked state do
;; Let T be that unmarked state
;; Mark T
;; for each a in % do
;; S = #-Closure(MoveNFA(T,a))
;; if S is not in SDFA already then
;; Add S to SDFA (as an “unmarked” state)
;; endIf
;; Set MoveDFA(T,a) to S
;; endFor
;; endWhile
;; for each S in SDFA do
;; if any s&S is a final state in the NFA then
;; Mark S an a final state in the DFA
;; endIf
;; endFor

(define (dfa-next-state dfa state char) 
  (matrix-ref dfa state (char->integer char)))

;; returns the index in the string
;; that is accepted by the dfa
(define (dfa-accept dfa input start-index)
  (newline) 
  (display (string-append "running dfa: on [" 
			  input 
			  "] start-index: " 
			  (number->string start-index)))
  (newline)
  (let ((input-length (string-length input)))
    (assert (< start-index input-length) 
	    "dfa-accept: start-index must less than string length")
    (let loop ((state start-state-index)
	       (input-index start-index))
      (newline)
      (display (string-append "state:" 
			      (number->string state)
			      " index: " 
			      (number->string input-index)))
      (newline)
      (cond ((= state end-state-index) input-index)
	    ((= input-index input-length) -1) ; did not match
	    (else (loop (dfa-next-state dfa 
					state 
					(string-ref input input-index))
			(inc input-index)))))))


;; test
(define a (make-nfa #\a))
(define b (make-nfa #\b))
(define c (nfa-concat a b))