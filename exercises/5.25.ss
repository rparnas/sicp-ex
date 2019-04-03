#|

Exercise 5.25: Modify the evaluator so that it uses
normal-order evaluation, based on the lazy evaluator of
Section 4.2.

|#

(load-ex "5.22")
(load-ex "4.27")

#| Answer

Copy-pasted the evaluator from 5.23 and modified by referencing 4.27.

Could have used some direction as to what should be primitive. I guess "allow
anything that doesn't call "eval"?

|#

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

;;; thunk code

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) 
  (cadr evaluated-thunk))

(define (eval-machine) (make-machine
  (list
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
    (list 'cond-actions cond-actions)
    ;;; exercise 5.25
    (list 'delay-it delay-it)
    (list 'thunk? thunk?)
    (list 'thunk-exp thunk-exp)
    (list 'thunk-env thunk-env)
    (list 'evaluated-thunk? evaluated-thunk?)
    (list 'thunk-value thunk-value)
    (list 'set-car! set-car!)
    (list 'set-cdr! set-cdr!))
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

    ;;; 5.25

    ;;; (actual-value (reg exp) (reg env)) => (reg val)
    actual-value
      (save continue)
      (assign continue (label actual-value-after-eval))
      (goto (label eval-dispatch))
    actual-value-after-eval
      (assign exp (reg val))
      (restore continue)
      (goto (label force-it)) ; tail-call to force-it

    ; (force-it (reg exp)) => (reg val)
    force-it
      (test (op thunk?) (reg exp))
      (branch (label force-it-thunk-case))
      (test (op evaluated-thunk?) (reg exp))
      (branch (label force-it-evaluated-thunk-case))
      (assign val (reg exp))
      (goto (reg continue))
    force-it-thunk-case
      (save continue)
      (assign continue (label force-it-thunk-case-0))
      (save exp)
      (assign env (op thunk-env) (reg exp))
      (assign exp (op thunk-exp) (reg exp))
      (goto (label actual-value))
    force-it-thunk-case-0
      (restore exp)
      (restore continue)
      (perform (op set-car!) (reg exp) (const evaluated-thunk)) ; exp is exp
      (assign exp (op cdr) (reg exp)) ; exp is (cdr exp)
      (perform (op set-car!) (reg exp) (reg val))
      (perform (op set-cdr!) (reg exp) (const ()))
      (goto (reg continue)) ; val is already assigned from actual-value call.
    force-it-evaluated-thunk-case
      (assign val (op thunk-value) (reg exp))
      (goto (reg continue))

    list-of-arg-values ; (list-of-arg-values (reg argl) (reg env)) => (reg val)
      (test (op no-operands?) (reg argl))
      (branch (label list-of-arg-values-base))
      ;;; get actual value of head
      (save continue)
      (save argl)
      (save env)
      (assign continue (label list-of-arg-values-after-head))
      (assign exp (op first-operand) (reg argl))
      (goto (label actual-value))
    list-of-arg-values-after-head
      ;;; get tail
      (restore env)
      (restore argl)
      (restore continue)
      (save continue)
      (save val)
      (assign continue (label list-of-arg-values-after-tail))
      (assign argl (op rest-operands) (reg argl))
      (goto (label list-of-arg-values))
    list-of-arg-values-after-tail
      (assign exp (reg val)) ; exp is tail
      (restore val) ; val is head
      (restore continue)
      (assign val (op cons) (reg val) (reg exp))
      (goto (reg continue))
    list-of-arg-values-base
      (assign val (const ()))
      (goto (reg continue))

    list-of-darg-values ; (list-of-arg-values (reg argl) (reg env)) => (reg val)
      (test (op no-operands?) (reg argl))
      (branch (label list-of-darg-values-base))
      ;;; get delayed value of head
      (assign val (op first-operand) (reg argl))
      (assign val (op delay-it) (reg val) (reg env))
      ;;; get tail
      (save val)
      (save continue)
      (assign continue (label list-of-darg-values-after-tail))
      (assign argl (op rest-operands) (reg argl))
      (goto (label list-of-darg-values))
    list-of-darg-values-after-tail
      (assign exp (reg val)) ; exp is tail
      (restore continue)
      (restore val) ; val is head
      (assign val (op cons) (reg val) (reg exp))
      (goto (reg continue))
    list-of-darg-values-base
      (assign val (const ()))
      (goto (reg continue))

    ; (apply (reg proc) (reg argl) (reg env))
    apply
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))  
      (branch (label compound-apply))
      (goto (label unknown-procedure-type))
    primitive-apply
      (save continue)
      (save proc)
      (assign continue (label primitve-apply-after-loa))
      (goto (label list-of-arg-values))
    primitve-apply-after-loa
      (restore proc)
      (restore continue)
      (assign argl (reg val))
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
    compound-apply
      (save continue)
      (save proc)
      (assign continue (label compound-apply-after-loda))
      (goto (label list-of-darg-values))
    compound-apply-after-loda
      (restore proc)
      (restore continue)
      (assign argl (reg val))
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (save continue)
      (goto (label ev-sequence))

    ev-application
      (save continue)
      (save exp)
      (save env)
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator))
      (goto (label actual-value))
    ev-appl-did-operator
      (restore env) ; environment for apply
      (restore exp)
      (restore continue)
      (assign proc (reg val)) ; operator for apply
      (assign argl (op operands) (reg exp)) ; operands for apply
      (goto (label apply))

    ;;; =================

    ;;; 5.4.2 Sequence Evaluation and Tail Recursion
    ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))
    ev-sequence
      (assign exp (op first-exp) (reg unev))
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-last-exp
      (restore continue)
      (goto (label eval-dispatch))

    ;;; 5.4.3 Conditionals, Assignments, and Definitions
    ev-if
      (save exp)                    ; save expression for later
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label actual-value))  ; evaluate the predicate -- modified for 5.25.
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
    done
    ;;; exercise 5.25 -- ensure final value is forced
    (assign continue (label final))
    (assign exp (reg val))
    (goto (label force-it))
    final)))

#| Tests -- infrastructure |#
(define (eval-one exp)
  (let ([m (eval-machine)])
    (set-register-contents! m 'exp exp)
    (set-register-contents! m 'env (setup-environment))
    (start m)
    (get-register-contents m 'val)))

#| Tests 

The failing regression tests are just becuase the following were not implemented:
  * do-while
  * for
  * letrec

The regression test from 4.27 passes and it only passes if the evaluator uses
normal-order evaluation.

The most important thing is that the test from 4.27 passes, be

|#
