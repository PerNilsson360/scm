;; Specification
;; The syntax of a record-type definition is: 
;;  <command or definition>           
;;    -> <record type definition>           ; addition to 8.1.6 in R5RS

;;  <record type definition>
;;    -> (define-record-type <type name>
;; 	(<constructor name> <field tag> ...)
;; 	<predicate name>
;; 	<field spec> ...)

;;  <field spec> -> (<field tag> <accessor name>)
;;               -> (<field tag> <accessor name> <modifier name>)

;;  <field tag> -> <identifier>
;;  <... name>  -> <identifier>

;; DEFINE-RECORD-TYPE is generative: each use creates a new record type that is distinct from all existing types, including other record types and Scheme's predefined types.
;; Record-type definitions may only occur at top-level (there are two possible semantics for `internal' record-type definitions, generative and nongenerative, and no consensus as to which is better). 
;; An instance of DEFINE-RECORD-TYPE is equivalent to the following definitions: 

;; <type name> is bound to a representation of the record type itself. Operations on record types, such as defining print methods, reflection, etc. are left to other SRFIs. 
;; <constructor name> is bound to a procedure that takes as many arguments as there are <field tag>s in the (<constructor name> ...) subform and returns a new <type name> record.
;; Fields whose tags are listed with <constructor name> have the corresponding argument as their initial value. The initial values of all other fields are unspecified. 
;; <predicate name> is a predicate that returns #T when given a value returned by <constructor name> and #F for everything else. 
;; Each <accessor name> is a procedure that takes a record of type <type name> and returns the current value of the corresponding field.
;; It is an error to pass an accessor a value which is not a record of the appropriate type. 
;; Each <modifier name> is a procedure that takes a record of type <type name> and a value which becomes the new value of the corresponding field; an unspecified value is returned.
;; It is an error to pass a modifier a first argument which is not a record of the appropriate type. 
;; Records are disjoint from the types listed in Section 4.2 of R5RS. 

;; Set!ing the value of any of these identifiers has no effect on the behavior of any of their original values. 

;; The following 

;;   (define-record-type :pare
;;     (kons x y)
;;     pare?
;;     (x kar set-kar!)
;;     (y kdr))

;; defines KONS to be a constructor, KAR and KDR to be accessors, SET-KAR! to be a modifier, and PARE? to be a predicate for :PAREs. 
;;   (pare? (kons 1 2))        --> #t
;;   (pare? (cons 1 2))        --> #f
;;   (kar (kons 1 2))          --> 1
;;   (kdr (kons 1 2))          --> 2
;;   (let ((k (kons 1 2)))
;;     (set-kar! k 3)
;;     (kar k))                --> 3

;; Implementation
;; This code is divided into three layers. In top-down order these are: 
;; Syntax definitions for DEFINE-RECORD-TYPE and an auxillary macro. 
;; An implementation of record types with a procedural interface. Some Scheme implementations already have something close to this. 
;; Vector-like records implemented in R5RS. This redefines some standard Scheme procedures and therefor must be loaded before any other code, including part 2 above.
;; Note that these procedures can be used to break the record-type abstraction (for example, RECORD-SET! can be used to modify the type of a record). Access to these procedures should be restricted. 
;; Syntax definitions

; We define the following procedures:
; 
; (make-record-type <type-name> <field-names>)    -> <record-type>
; (record-constructor <record-type> <field-names>) -> <constructor>
; (record-predicate <record-type>)               -> <predicate>w
; (record-accessor <record-type> <field-name>)    -> <accessor>
; (record-modifier <record-type> <field-name>)    -> <modifier>
;   where
; (<constructor> <initial-value> ...)         -> <record>
; (<predicate> <value>)                       -> <boolean>
; (<accessor> <record>)                       -> <value>
; (<modifier> <record> <value>)         -> <unspecific>

; Record types are implemented using vector-like records.  The first
; slot of each record contains the record's type, which is itself a
; record.

;----------------
; Record types are themselves records, so we first define the type for
; them.  Except for problems with circularities, this could be defined as:
;  (define-record-type :record-type
;    (make-record-type name field-tags)
;    record-type?
;    (name record-type-name)
;    (field-tags record-type-field-tags))
;
; As it is, we need to define everything by hand.
; This implements a record abstraction that is identical to vectors,
; except that they are not vectors (VECTOR? returns false when given a
; record and RECORD? returns false when given a vector).  The following
; procedures are provided:
;
;   (record? <value>)                -> <boolean>
;   (make-record <size>)             -> <record>
;   (record-ref <record> <index>)    -> <value>
;   (record-set! <record> <index> <value>) -> <unspecific>
;
; These can implemented in R5RS Scheme as vectors with a distinguishing
; value at index zero, providing VECTOR? is redefined to be a procedure
; that returns false if its argument contains the distinguishing record
; value.  EVAL is also redefined to use the new value of VECTOR?.

; Define the marker and redefine VECTOR? and EVAL.

(define (make-record type slots)
  (let ((result (make-vector (+ (length slots) 1))))
    (vector-set! result 0 (list 'srfi9-record type slots))
    result))

(define (record? obj)
  (and (vector? obj) 
       (> (vector-length obj) 0)
       (pair? (vector-ref obj 0))
       (eq? (car (vector-ref obj 0)) 'srfi9-record)))

(define (record-symbol-list record) (caddr (vector-ref record 0)))

(define (record-type record) 
  (if (not (record? record))
      (error "RECORD-TYPE: not a record")
      (cadr (vector-ref record 0))))

(define (record-type? record type) 
  (if (not (record? record))
      #f
      (eq? (cadr (vector-ref record 0)) type)))

(define (symbol-index symbols symbol)
  (let loop ((index 0)
	     (s symbols))
    (cond ((null? s) (error "RECORD: could not find index, bug!"))
	  ((eq? (car s) symbol) (+ index 1))
	  (else (loop (+ index 1) (cdr s))))))

(define (record-set! obj field value)
  (cond ((not (record? obj)) (error "RECORD-SET!: obj is not a record"))
	((not (symbol? field)) (error "RECORD-SET!: field is not a symbol"))
	(else (let ((index (symbol-index (record-symbol-list obj) field)))
		(vector-set! obj index value)))))

(define (record-values-set! obj values)
  (let ((names (record-symbol-list obj)))
    (if (not (= (length names) (length values)))
	(error "RECORD: must have same number of values as fields")
	(let loop ((n names)
		   (v values))
	  (if (not (null? n))
	      (begin (record-set! obj (car n) (car v)) 
		     (loop (cdr n) (cdr v))))))))

(define (record-ref obj field)
  (cond ((not (record? obj)) (error "RECORD-REF!: obj is not a record"))
	((not (symbol? field)) (error "RECORD-REF!: field is not a symbol"))
	(else (let ((index (symbol-index (record-symbol-list obj) field)))
		(vector-ref obj index)))))

(define p (make-record '<point> '(x y)))
(record-set! p 'x 1)
(record-set! p 'y 2)
(record-ref p 'x)
(record-ref p 'y)

(define (accessor-name type slot-name)
  (string->symbol (string-append (symbol->string type) 
				 ":" 
				 (symbol->string slot-name))))

(define (mutator-name type slot-name)
  (string->symbol (string-append (symbol->string type) 
				 ":" 
				 (symbol->string slot-name)
				 "!")))

(define (define-accessor-and-mutators type slot-names)
  (if (not (null? slot-names))
      (let ((slot-name (car slot-names)))
	(begin (eval `(define (,(accessor-name type slot-name) record)
					(record-ref record ',slot-name))
				 environment)
	       (eval `(define (,(mutator-name type slot-name) record value)
					(record-set! record ',slot-name value))
				 environment)
	       (define-accessor-and-mutators type (cdr slot-names))))))

(define (record-predicate-name type)
  (string->symbol (string-append (symbol->string type) "?")))

;; Constructs a method with name <type>? to check if object is a record of type
(define (define-record-predicate type)
  (eval `(define (,(record-predicate-name type) record)
		   (record-type? record ',type))
		environment))

(define (define-record-type type ctor-spec)
  (if (not (pair? ctor-spec))
      (error "DEFINE-RECORD-TYPE: ctor spec is not a list")
      (let ((ctor (car ctor-spec))
	    (slot-names (cdr ctor-spec)))
	(eval `(define (,ctor . slot-values) 
			 (let ((r (make-record (quote ,type) (quote ,slot-names))))
			   (record-values-set! r slot-values)
		       r))
	      environment)
	(define-accessor-and-mutators type slot-names)
	(define-record-predicate type))))
 
(define-record-type 
  '<point> 
  '(make-point x y)
  ;; '(translate (self delta)
  ;;  			  (begin (<point>:x! self (+ (<point>:x self) delta))
  ;;  					 (<point>:y! self (+ (<point>:y self) delta))))
  )
		     
