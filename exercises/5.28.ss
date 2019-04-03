#|

Exercise 5.28: Modify the definition of the evaluator by
changing "eval-sequence" as described in Section 5.4.2 so
that the evaluator is no longer tail-recursive. Rerun your
experiments from Exercise 5.26 and Exercise 5.27 to
demonstrate that both versions of the "factorial" procedure
now require space that grows linearly with their input.

|#

#| Answer 

Iterative factorial (w/o tail recursion) suffers the most, which makes sense
because it is the algorithm that was relying on tail recursive in order to make
the process iterative.

recursive factorial (w/ tail recursion)
  * pushes: 32n - 10
  * max depth: 5n + 3

iterative factorial (w/ tail recursion)
  * pushes: (n+1) * 35
  * max depth: 10

recursive factorial (w/o tail recursion)
  * pushes: 34n - 8
  * max depth: 8n + 6

iterative factorial (w/o tail recursion)
  * pushes: 37n + 41
  * max depth: 3n + 17

> (map factorial-iter-test (cdr (iota 10)))
(("1!" 1 . "pushes=78, max-depth=20") 
 ("2!" 2 . "pushes=115, max-depth=23")
 ("3!" 6 . "pushes=152, max-depth=26")
 ("4!" 24 . "pushes=189, max-depth=29")
 ("5!" 120 . "pushes=226, max-depth=32")
 ("6!" 720 . "pushes=263, max-depth=35")
 ("7!" 5040 . "pushes=300, max-depth=38")
 ("8!" 40320 . "pushes=337, max-depth=41")
 ("9!" 362880 . "pushes=374, max-depth=44"))

> (map factorial-recur-test (cdr (iota 10)))
(("1!" 1 . "pushes=26, max-depth=14") 
 ("2!" 2 . "pushes=60, max-depth=22")
 ("3!" 6 . "pushes=94, max-depth=30")
 ("4!" 24 . "pushes=128, max-depth=38")
 ("5!" 120 . "pushes=162, max-depth=46")
 ("6!" 720 . "pushes=196, max-depth=54")
 ("7!" 5040 . "pushes=230, max-depth=62")
 ("8!" 40320 . "pushes=264, max-depth=70")
 ("9!" 362880 . "pushes=298, max-depth=78"))

|#

#| Answer -- copy-paste of 5.26 and 5.27 |#
(define (factorial-iter-test n)
  (define fact-exp
    `(begin 
      (define (factorial n)
        (define (iter product counter)
          (if (> counter n)
              product
              (iter (* counter product) (+ counter 1))))
        (iter 1 1))
      (factorial ,n)))
  (cons (format "~a!" n) (eval-stack-stats fact-exp)))

(define (eval-stack-stats exp)
  (let* ([m (eval-machine)])
    (set-register-contents! m 'exp exp)
    (set-register-contents! m 'env (setup-environment))
    (start m)
    (cons (get-register-contents m 'val)
          ((m 'stack) 'get-statistics))))


(define (factorial-recur-test n)
  (define fact-exp
    `(begin 
      (define (factorial n)
        (if (= n 1)
            1 
            (* (factorial (- n 1)) n)))
      (factorial ,n)))
  (cons (format "~a!" n) (eval-stack-stats fact-exp)))

#| Answer -- Everything below is a rewrite of 5.23 |#

(load-ex "5.22")
(load-ex "4.7")

#| Answer + Code from book 

  * For 5.23:
    * from 4.1: cond
    * from 4.4: and, or
    * from 4.5: cond (arrow syntax)
    * from 4.6: let
    * from 4.7: let*
|#

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

(define (eval-machine) (make-machine
  (list
    (list 'no-more-exps? no-more-exps?)
    (list 'adjoin-arg adjoin-arg)
    (list 'application? application?)
    (list 'apply-primitive-procedure apply-primitive-procedure)
    (list 'assignment? assignment?)
    (list 'assignment-variable assignment-variable)
    (list 'assignment-value assignment-value)
    (list 'begin-actions begin-actions)
    (list 'begin? begin?)
    (list 'compound-procedure? compound-procedure?)
    (list 'define-variable! define-variable!)
    (list 'definition? definition?)
    (list 'definition-value definition-value)
    (list 'definition-variable definition-variable)
    (list 'empty-arglist empty-arglist)
    (list 'error error)
    (list 'extend-environment extend-environment)
    (list 'first-exp first-exp)
    (list 'first-operand first-operand)
    (list 'if? if?)
    (list 'if-alternative if-alternative)
    (list 'if-consequent if-consequent)
    (list 'if-predicate if-predicate)
    (list 'lambda? lambda?)
    (list 'lambda-parameters lambda-parameters)
    (list 'lambda-body lambda-body)
    (list 'last-exp? last-exp?)
    (list 'last-operand? last-operand?)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'make-procedure make-procedure)
    (list 'no-operands? no-operands?)
    (list 'operands operands)
    (list 'operator operator)
    (list 'procedure-body procedure-body)
    (list 'procedure-parameters procedure-parameters)
    (list 'procedure-environment procedure-environment)
    (list 'primitive-procedure? primitive-procedure?)
    (list 'rest-exps rest-exps)
    (list 'rest-operands rest-operands)
    (list 'self-evaluating? self-evaluating?)
    (list 'set-variable-value! set-variable-value!)
    (list 'text-of-quotation text-of-quotation)
    (list 'true? true?)
    (list 'quoted? quoted?)
    (list 'variable? variable?)
    ;;; exercise 5.23
    (list 'cond? cond?)
    ; (list 'cond->if cond->if)
    (list 'is-and? is-and?)
    (list 'is-or? is-or?)
    (list 'and-clauses and-clauses)
    (list 'or-clauses or-clauses)
    (list 'eval-and eval-and)
    (list 'eval-or eval-or)
    (list 'is-let? is-let?)
    (list 'let->combination let->combination)
    (list 'is-let*? is-let*?)
    (list 'let*->nested-lets let*->nested-lets)
    ;;; exercise 5.24
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'cond-clauses cond-clauses)
    (list 'cond-else-clause? cond-else-clause?)
    (list 'cond-predicate cond-predicate)
    (list 'cond-actions cond-actions))
  '((assign continue (label done)) ; custom

    ;;; 5.4.1 The Core of the Explicit Control Evaluator
    eval-dispatch
      (test (op self-evaluating?) (reg exp))
      (branch (label ev-self-eval))
      (test (op variable?) (reg exp))
      (branch (label ev-variable))
      (test (op quoted?) (reg exp))
      (branch (label ev-quoted))
      (test (op assignment?) (reg exp))
      (branch (label ev-assignment))
      (test (op definition?) (reg exp))
      (branch (label ev-definition))
      (test (op if?) (reg exp))
      (branch (label ev-if))
      (test (op lambda?) (reg exp))
      (branch (label ev-lambda))
      (test (op begin?) (reg exp))
      (branch (label ev-begin))
      ;;; exercise 5.23
      (test (op cond?) (reg exp))
      (branch (label ev-cond))
      (test (op is-and?) (reg exp))
      (branch (label ev-and))
      (test (op is-or?) (reg exp))
      (branch (label ev-or))
      (test (op is-let?) (reg exp))
      (branch (label ev-let))
      (test (op is-let*?) (reg exp))
      (branch (label ev-let*))
      ;;; =============
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))

    ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))

    ev-variable
      (assign val (op lookup-variable-value) (reg exp) (reg env))
      (goto (reg continue))

    ev-quoted
      (assign val (op text-of-quotation) (reg exp))
      (goto (reg continue))

    ev-lambda
      (assign unev (op lambda-parameters) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
      (goto (reg continue))

    ev-application
      (save continue)
      (save env)
      (assign unev (op operands) (reg exp))
      (save unev)
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator))
      (goto (label eval-dispatch))
    ev-appl-did-operator
      (restore unev)                  ; the operands
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val))         ; the operator
      (test (op no-operands?) (reg unev))
      (branch (label apply-dispatch))
      (save proc)
    ev-appl-operand-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-appl-accumulate-arg))
      (goto (label eval-dispatch))
    ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label eval-dispatch))
    ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label apply-dispatch))

    apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))  
      (branch (label compound-apply))
      (goto (label unknown-procedure-type))
    primitive-apply
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (restore continue)
      (goto (reg continue))
    compound-apply
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))

    ;;; 5.4.2 Sequence Evaluation and Tail Recursion
    ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))
    ev-sequence
      (test (op no-more-exps?) (reg unev))
      (branch (label ev-sequence-end))
      (assign exp (op first-exp) (reg unev))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-end
      (restore continue)
      (goto (reg continue))

    ;;; 5.4.3 Conditionals, Assignments, and Definitions
    ev-if
      (save exp)                    ; save expression for later
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch))  ; evaluate the predicate
    ev-if-decide
      (restore continue)
      (restore env)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-if-consequent))
    ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))
    ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))

    ev-assignment
      (assign unev (op assignment-variable) (reg exp))
      (save unev)                   ; save variable for later
      (assign exp (op assignment-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-assignment-1))
      (goto (label eval-dispatch))  ; evaluate the assignment value
    ev-assignment-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

    ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev)                   ; save variable for later
      (assign exp (op definition-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch))  ; evaluate the definition value
    ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op define-variable!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

    ;;; exercise 5.23
    ;ev-cond
    ;(assign exp (op cond->if) (reg exp))
    ;(goto (label eval-dispatch))

    ev-and
    (assign exp (op and-clauses) (reg exp))
    (assign val (op eval-and) (reg exp) (reg env))
    (goto (reg continue))

    ev-or
    (assign exp (op or-clauses) (reg exp))
    (assign val (op eval-or) (reg exp) (reg env))
    (goto (reg continue))

    ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))

    ev-let*
    (assign exp (op let*->nested-lets) (reg exp))
    (goto (label eval-dispatch))

    ;;; =============

    ;;; exercise 5.24
    ev-cond
    (assign exp (op cond-clauses) (reg exp))   ; exp is clauses
    ev-cond-loop
    (assign val (op car) (reg exp))            ; val is 1st clause
    (test (op cond-else-clause?) (reg val))    ; if 1st clause is else
    (branch (label ev-cond-evaluate-actions))  ;   eval 1st clause actions
    (save continue)
    (save exp)
    (assign continue (label ev-cond-after-pred))                           
    (assign exp (op cond-predicate) (reg val))
    (goto (label eval-dispatch))
    ev-cond-after-pred
    (restore exp)                              ; exp is clauses
    (restore continue)
    (test (op true?) (reg val))                ; if the predicate is true
    (branch (label ev-cond-evaluate-actions))  ;  eval 1st clause actions
    (assign exp (op cdr) (reg exp))
    (goto (label ev-cond-loop))

    ev-cond-evaluate-actions
    (assign exp (op car) (reg exp))                ; exp is 1st clause
    (assign exp (op cond-actions) (reg exp))       ; exp is the cond-actions
    (assign exp (op cons) (const begin) (reg exp)) ; exp is actions prepended with "begin"
    (goto (label eval-dispatch))
    ;;; =============

    ;;; 5.4.4 Running the Evaluator -- modified
    unknown-expression-type
      (perform (op error) (const "unknown expression type") (reg exp))
    unknown-procedure-type
      (perform (op error) (const "unknown procedure type") (reg proc))
    done)))

#| Tests

Redefining eval-one causes regression tests to use the the explicit control
evaluator.

Currently one test fails because I haven't implemented "cond arrow syntax"
from 4.5 as part of doing the raw cond implementation of 5.24.

|#

(define (eval-one exp)
  (let ([m (eval-machine)])
    (set-register-contents! m 'exp exp)
    (set-register-contents! m 'env (setup-environment))
    (start m)
    (get-register-contents m 'val)))