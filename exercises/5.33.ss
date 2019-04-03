#|

Exercise 5.33: Consider the following definition of a
factorial procedure, which is slightly different from the
one given above:

(define (factorial-alt n)
  (if (= n 1)
      1
      (* n (factorial-alt (- n 1)))))

Compile this procedure and compare the resulting code with
that produced for "factorial". Explain any differences you
find. Does either program execute more efficiently than the
other?

|#

(define built-in-apply apply)

(load-ex "5.32")

;;; NOTE: The book does not warn that operators must support label arguments (?)

;;; modified from 5.9
(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp) operations)]
        [aprocs
         (map (lambda (e)
                (if (or (register-exp? e) (constant-exp? e) (label-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "assemble" 
                           "op argument must be register, constant, or label"
                           e)))
              (operation-exp-operands exp))])
    (lambda ()
      (built-in-apply op (map (lambda (p) (p)) aprocs))))) ; NOTE: only use original version of apply

#| Code from book |#

;;; target: register to place result value
;;; linkage: what to do after evaluation finishes
;;;          - next: continue at the next instruction in sequence
;;;          - return: return from the procedure being compiled
;;;          - jump: to a named entry point (by label)
(define (comp exp target linkage)
  (cond [(self-evaluating? exp)
         (compile-self-evaluating exp target linkage)]
        [(quoted? exp) (compile-quoted exp target linkage)]
        [(variable? exp)
         (compile-variable exp target linkage)]
        [(assignment? exp)
         (compile-assignment exp target linkage)]
        [(definition? exp)
         (compile-definition exp target linkage)]
        [(if? exp) (compile-if exp target linkage)]
        [(lambda? exp) (compile-lambda exp target linkage)]
        [(begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage)]
        [(cond? exp) (comp (cond->if exp) target linkage)]
        [(application? exp)
         (compile-application exp target linkage)]
        [else
         (error "compile" "unknown expression type" exp)]))

;;; needs: regs that must be initalized before the sequence is executed
;;; modifies: regs that are modified by the sequence
;;; statements: the actual instructions of the sequence
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (comp (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (comp (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ label-counter 1))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (comp (if-predicate exp) 'val 'next))
            (c-code
             (comp (if-consequent exp) target consequent-linkage))
            (a-code
             (comp (if-alternative exp) target linkage)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (comp (first-exp seq) target linkage)
      (preserving '(env continue)
       (comp (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-application exp target linkage)
  (let ((proc-code (comp (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (comp operand 'val 'next))
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

(define all-regs '(env proc val argl continue))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry) (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "compile" "return linkage, target not val" target))))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

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

#| Answer

Neither is more efficient than the other. The differences involve saves,
assigns, and restores of identical complexity, just moved around.

(define (factorial n)             | (define (factorial n)
  (if (= n 1)                     |   (if (= n 1)
      1                           |       1
      (* (factorial (- n 1)) n))) |       (* n (factorial (- n 1)))))

I used the display-asm utility below, copy-pasted the results to two different
files and used a diff program.

The first difference is when the arguments for multiple begin to be evaluated. 

The original begins building up the argument list by adding n to it. Next it
saves argl (because to get the next argument we must invoke functions and the
arguments for functions are stored in argl). It doesn't need to save env, as
evaluating the arguments of the recursive factorial call involves only primitive
functions and variable lookups.

The alternative begins building up its argument list by performing a recursive
call. It saves env, because of the recursive call, but it doesn't need to save
argl because it hasn't yet begun building up the argument list. 

 | (assign proc (op lookup-variable-value) (const *) (reg env))          | (assign proc (op lookup-variable-value) (const *) (reg env)) 
 | (save continue)                                                       | (save continue)
 | (save proc)                                                           | (save proc)
!| (assign val (op lookup-variable-value) (const n) (reg env))           |
!| (assign argl (op list) (reg val))                                     | 
!| (save argl)                                                           | (save env)
 | (assign proc (op lookup-variable-value) (const factorial) (reg env))  | (assign proc (op lookup-variable-value) (const factorial) (reg env))

The second difference is after the recursive factorial call right before
multiple is about to be invoked. The original restores the argument list and
adds the value of the recursive call to it as the final argument. The
alternative begins the argument list with the value of the recursive call,
restores the environment, and looks up the value of n, which is the final
argument in its argument list.

! |                                              | (assign argl (op list) (reg val))
! | (restore argl)                               | (restore env)
! |                                              | (assign val (op lookup-variable-value) (const n) (reg env))
  | (assign argl (op cons) (reg val) (reg argl)) | (assign argl (op cons) (reg val) (reg argl))

|#

#| Tests -- infrastructure |#
(define (compile-test exp)
  (set! label-counter 0)
  (let* ([compiled-exp (comp exp 'val 'next)]
         [m (make-machine 
              (list
                (list 'apply-primitive-procedure apply-primitive-procedure)
                (list 'cons cons)
                (list 'compound-procedure? compound-procedure?) ; added for 5.47
                (list 'compiled-procedure-env compiled-procedure-env)
                (list 'compiled-procedure-entry compiled-procedure-entry)
                (list 'define-variable! define-variable!)
                (list 'extend-environment extend-environment)
                (list 'false? false?)
                (list 'list list)
                (list 'lookup-variable-value lookup-variable-value)
                (list 'make-compiled-procedure make-compiled-procedure)
                (list 'primitive-procedure? primitive-procedure?)
                (list 'set-car! set-car!) ; added for 5.36
                (list 'set-cdr! set-cdr!) ; added for 5.36
                (list '= (lambda (a b) (if (= a b) 'true 'false))) ; added for 5.38
                (list '* *) ; added for 5.38
                (list '- -) ; added for 5.38
                (list '+ +) ; added for 5.38
                (list 'set-variable-value! set-variable-value!)) 
              (statements compiled-exp))])
    (set-register-contents! m 'env (setup-environment))
    (start m)
    (get-register-contents m 'val)))

(define (display-asm exp)
  (set! label-counter 0)
  (let ([compiled-exp (comp exp 'val 'next)])
    (for-each (lambda (i)
                (newline)
                (if (not (symbol? i))
                    (display "  ")
                    (void))
                (display i))
              (statements (comp exp 'val 'next)))))

(define easy-prog
  '(begin
     (define (func) 0)
     (func)))

(define factorial-2-prog
  '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
    (factorial 2)))

(define factorial-5-prog
  '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
    (factorial 5)))

(define factorial-alt-2-prog
  '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))
    (factorial 2)))

(define factorial-alt-5-prog
  '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))
    (factorial 5)))

(define-test (compile-test easy-prog) 0)

(define-test (compile-test factorial-2-prog) 2)

(define-test (compile-test factorial-alt-2-prog) 2)

(define-test (compile-test factorial-5-prog) 120)

(define-test (compile-test factorial-alt-5-prog) 120)
