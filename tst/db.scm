;; A db has a name a schema and tables
(define (make-db name schema)
  (let ((db (make-vector 3)))
    (vector-set! db 0 name)
    (vector-set! db 1 schema)
    db))

(define (db-name db) (vector-ref db 0))
(define (db-schema db) (vector-ref db 1))
(define (db-table db) (vector-ref db 2))

;; db schema has a name and 0 or more tupple-schemas
;; tupple-schema has a name and 0 or more attribute-schemas
;; attribute-schemas has name, index, flags for key and foreign-key
;; foreign key is a pair with a table name and a attribute name
(define (db-attribute-schema name index key foreign-key)
  (let ((attribute-schema (make-vector 4)))
    (vector-set! attribute-schema 0 name)
    (vector-set! attribute-schema 1 index)
    (vector-set! attribute-schema 2 key)
    (vector-set! attribute-schema 3 foreign-key)
    attribute-schema))

(define (db-attribute-name db-attribute) (vector-ref db-attribute 0))
(define (db-attribute-index db-attribute) (vector-ref db-attribute 1))
(define (db-attribute-key? db-attribute) (vector-ref db-attribute 2))
(define (db-attribute-fk db-attribute) (vector-ref db-attribute 3))
