#|

Exercise 5.47: This section described how to modify the
explicit-control evaluator so that interpreted code can call
compiled procedures. Show how to modify the compiler so that
compiled procedures can call not only primitive procedures
and compiled procedures, but interpreted procedures as well.
This requires modifying "compile-procedure-call" to handle
the case of compound (interpreted) procedures. Be sure to
handle all the same "target" and "linkage" combinations as
in "compile-proc-appl". To do the actual procedure
application, the code needs to jump to the evaluator's
"compound-apply" entry point. This label cannot be directly
referenced in object code (since the assembler requires that
all labels referenced by the code it is assembling be
defined there), so we will add a register called "compapp"
to the evaluator machine to hold this entry point, and add
an instruction to initialize it:

 (assign compapp (label compound-apply))
 (branch (label external-entry))  ; branches if "flag" is set 
read-eval-print-loop   ... 

To test your code, start by defining a procedure "f" that
calls a procedure "g". Use "compile-and-go" to compile the
definition of "f" and start the evaluator. Now, typing at
the evaluator, define "g" and try to call "f".

|#

(load-ex "5.33") ; skip over lexical analysis

#| Answer |#

;;; rewrite from 5.33
(define (compile-procedure-call target linkage)
  (let ([primitive-branch (make-label 'primitive-branch)]
        [compound-branch (make-label 'compound-branch)]
        [compiled-branch (make-label 'compiled-branch)]
        [after-call (make-label 'after-call)])
    (let ([compiled-linkage
           (if (eq? linkage 'next) after-call linkage)])
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (parallel-instruction-sequences
          (append-instruction-sequences
           compound-branch
           (compound-proc-appl target compiled-linkage))
          (append-instruction-sequences
           primitive-branch
           (end-with-linkage linkage
            (make-instruction-sequence '(proc argl) (list target)
             `((assign ,target
                       (op apply-primitive-procedure)
                       (reg proc)
                       (reg argl))))))))
       after-call))))

;;; based on 5.33 compile-proc-appl
(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (save continue)
             (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (save continue)
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "compile" "return linkage, target not val" target))))

#| Tests -- explicit-control-evaluator can run compiled procedures |#

(define (compile-and-go cexp exp)
  (let* ([m (eval-machine)]
         [instructions (assemble (statements (comp cexp 'val 'return)) m)])
    (set-register-contents! m 'val instructions)
    (set-register-contents! m 'flag #t)
    (set-register-contents! m 'exp exp)
    (set-register-contents! m 'env (setup-environment))
    (start m)
    (get-register-contents m 'val)))

(define-test (compile-and-go '(define (x) 7) '(x))
             7)

(define-test (compile-and-go
               '(define (factorial n)
                  (if (= n 1)
                      1
                      (* (factorial (- n 1)) n)))
               '(factorial 5))
             120)

#| Tests |#
(define-test (compile-and-go
               '(define (f) (g))
               '(begin
                  (define (g) 5)
                  (f)))
  5)