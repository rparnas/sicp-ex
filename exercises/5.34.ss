#|

Exercise 5.34: Compile the iterative factorial procedure

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

Annotate the resulting code, showing the essential
difference between the code for iterative and recursive
versions of "factorial" that makes one process build up
stack space and the other run in constant stack space.

|#

#| Answer 

> (reset-ex "5.33")
> (display-asm '(define (factorial n)
                  (define (iter product counter)
                    (if (> counter n)
                        product
                        (iter (* counter product)
                              (+ counter 1))))
                  (iter 1 1))))

Each additional iter call does not grow the stack. The compiler is missing a
common optimization where 

However the tail call does not have the optimization where instead of creating a
new env frame for each call, we  reuse the existing frame.

|#

;;; make procedure factorial
  (assign val (op make-compiled-procedure) (label entry24) (reg env))
  (goto (label after-lambda23))
;;; (factorial n) -- make env frame and iter. continue w/ procedure body at after-labmda28
entry24
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry29) (reg env))
  (goto (label after-lambda28))

;;; (iter n)
entry29
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)) ; about to evaluate (> counter n)
  (branch (label primitive-branch44))
compiled-branch43
  (assign continue (label after-call42))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch44
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call42 ; after evaluation of (> counter n)
  (restore env)
  (restore continue)
  (test (op false?) (reg val)) ; if
  (branch (label false-branch31))
true-branch32
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue)) ; return product
false-branch31
  (assign proc (op lookup-variable-value) (const iter) (reg env)) ; about to evaluate (iter ...)
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)) ; about to evaluate (+ counter 1)
  (branch (label primitive-branch38))
compiled-branch37
  (assign continue (label after-call36))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch38
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call36 ; after evaluating (+ counter 1)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)) ; about to evaluate (* counter product)
  (branch (label primitive-branch35))
compiled-branch34
  (assign continue (label after-call33))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch35
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call33 ; after evaluating (* counter product)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc)) ; about to evaluate recursive call
  (branch (label primitive-branch41))
compiled-branch40
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) ; go directly to entry29, a tail-call to iter. the stack is not any bigger.
primitive-branch41
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call39
after-if30
;;; body of factorial after defining iter -- tail-call invoke (iter 1 1)
after-lambda28
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch27))
compiled-branch26
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch27
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
;;; unused
after-call25
;;; save the procedure factorial in the environment and return 'ok
after-lambda23
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))