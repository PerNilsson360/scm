(define all-regs '(continue env val proc argl))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))


(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence) (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(reg_cont) '() "goto reg_cont;"))
        ((eq? linkage 'next) (empty-instruction-sequence))
        (else (make-instruction-sequence 
	       '() 
	       '() 
	       (list (string-append "goto " (symbol->string linkage) ";"))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(reg_cont) instruction-sequence (compile-linkage linkage)))


(define (compile-self-evaluating exp target linkage)
  (end-with-linkage 
   linkage
   (make-instruction-sequence 
    '() 
    (list target) 
    (list (string-append (symbol->string target) 
			 " = " 
			 (self-evaluating->string exp) 
			 ";")))))

(define (compile-quoted exp target linkage)
  (end-with-linkage 
   linkage
   (make-instruction-sequence 
    '() 
    (list target)
    (list (string-append (symbol->string target) 
			 " = quotation_value(" 
			 (symbol->string exp) 
			 ");")))))

(define (compile-variable exp target linkage)
  (end-with-linkage 
   linkage
   (make-instruction-sequence 
    '(env) 
    (list target)
    (list (string-append (symbol->string target) 
			 " = lookup_variable_value(" 
			 (symbol->string exp)
			 ", reg_env);")))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp) 'reg_val 'next)))
    (end-with-linkage 
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence 
       '(env val) 
       (list target)
       (list (string-append "set_variable_value(mk_symbol(\"" 
			    (symbol->string var)
			    "\"), reg_val, reg_env);")))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code (compile (definition-value exp) 'val 'next)))
    (end-with-linkage 
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence 
       '(env val) 
       (list target)
       (list (string-append "define_variable(mk_symbol(\"" 
			    (symbol->string var)
			    "\"), reg_val, reg_env);")))))))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code (compile (if-consequent exp) target consequent-linkage))
            (a-code (compile (if-alternative exp) target linkage)))
        (preserving 
	 '(env continue)
	 p-code
	 (append-instruction-sequences
	  (make-instruction-sequence 
	   '(val) 
	   '()
	   (list (string-append "if (!reg_val) goto " 
				(symbol->string f-branch) ";")))
	  (parallel-instruction-sequences
	   (append-instruction-sequences t-branch c-code)
	   (append-instruction-sequences f-branch a-code))
	  after-if))))))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence 
	  '(env) 
	  (list target)
	  (list (string-append 
		 (symbol->string target) 
		 " = make_compiled_procedure(&&" 
		 (symbol->string proc-entry)
		 ", reg_env);"))))
	 (compile-lambda-body exp proc-entry))
	after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl) 
      '(env)
      (string->append (symbol->string proc-entry) 
		      ":"
		      "reg_env = compiled_procedure_env(reg_proc);"
		      "reg_env = extend_environment("
		      formals
		      "arg1, reg_env);")
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))


(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))


(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))


(define (registers-needed s) (if (symbol? s) '() (car s)))
(define (registers-modified s) (if (symbol? s) '() (cadr s)))
(define (statements s) (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)(memq reg (registers-needed seq)))
(define (modifies-register? seq reg)(memq reg (registers-modified seq)))

(define (append-seq-list seqs)
  (if (null? seqs)
      (empty-instruction-sequence)
      (append-2-sequences (car seqs)
			  (append-seq-list (cdr seqs)))))

(define (append-2-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
	       (list-difference (registers-needed seq2)
				(registers-modified seq1)))
   (list-union (registers-modified seq1)
	       (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define (append-instruction-sequences . seqs) (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))


(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))


(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define (self-evaluating? exp) (or (number? exp)(string? exp)))
(define (self-evaluating->string exp) 
  (display exp)
  (cond ((number? exp) (number->string exp))
	((string? exp) exp)
	(else (error "SELF_EVALUATING_TO_STRING: wrong type"))))
				       
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag) (and (pair? exp) (eq? (car exp) tag)))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (list->nested-cons li)
  (cond  ))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

(define (match? exp) (tagged-list? exp 'match))
(define (match-exp exp) (cadr exp))
(define (match-clauses exp) (cddr exp))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

;; (compile
;;  '(define (factorial n)
;;     (if (= n 1)
;;         1
;;         (* (factorial (- n 1)) n)))
;;  'val
;;  'next)

(compile '(set! a 1) 'val 'next)

;; (compile
;;  '(lambda (a b c) 1)
;;  'val
;;  'next)

(compile '(+ 1 212 12) 'val 'next)