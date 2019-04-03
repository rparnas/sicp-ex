#|

Exercise 5.32: Using the "preserving" mechanism, the
compiler will avoid saving and restoring "env" around the
evaluation of the operator of a combination in the case
where the operator is a symbol. We could also build such
optimizations into the evaluator. Indeed, the
explicit-control evaluator of Section 5.4 already performs a
similar optimization, by treating combinations with no
operands as a special case.

a. Extend the explicit-control evaluator to recognize as a
separate class of expressions combinations whose operator is
a symbol, and to take advantage of this fact in evaluating
such expressions.

b. Alyssa P. Hacker suggests that by extending the evaluator
to recognize more and more special cases we could
incorporate all the compiler's optimizations, and that this
would eliminate the advantage of compilation altogether.
What do you think of this idea?

|#

#| Answer 

a. See below. Explicit-control evaluator copied from 5.23

b. This is not likely a good idea. The compiler is run once, and it is likely
you can be less picky about performance. Also the language of the compiler
closely matches the language of how you might want to talk about executing
scheme. Even before the compilation chapter, I found myself denoting subroutines
in terms of which registers they need initialized and which registers they
output to.

|#

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

(define (symbol-application? exp) 
  (and (pair? exp) (symbol? (car exp))))

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

(define (eval-machine)
  (let ([m (make-new-machine)])
    ((m 'install-operations)
      (list
        (list 'adjoin-arg adjoin-arg)
        (list 'application? application?)
        (list 'symbol-application? symbol-application?)
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
        (list 'false? false?)
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
        (list 'list list)
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
        (list 'cond-is-arrow? cond-is-arrow?)
        (list 'cond-arrow-proc cond-arrow-proc)
        ;;; exercise 5.47
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
        ;;; exercise 5.48
        (list 'is-compile? is-compile?)
        (list 'compile-exp compile-exp)
        (list 'compile (lambda (exp) (assemble (statements (comp exp 'val 'return)) m)))))
    ((m 'install-instructions-sequence)
     (assemble '(

    (assign compapp (label compound-apply))
    (branch (label external-entry)) ; 5.47

    regular-eval
    (assign continue (label done)) ; custom

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
      ;;; exercise 5.48
      (test (op is-compile?) (reg exp))
      (branch (label ev-compile))
      ;;; exercise 5.32
      (test (op symbol-application?) (reg exp))
      (branch (label ev-symbol-application))
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

    ev-symbol-application
      (save continue)
      (assign unev (op operands) (reg exp))
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator-no-restore))
      (goto (label eval-dispatch))

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
    ev-appl-did-operator-no-restore
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
      (test (op compiled-procedure?) (reg proc)) ; added for 5.47
      (branch (label compiled-apply)) ; added for 5.47
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
    compiled-apply ; added for 5.47
      (restore continue)
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))

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
    (assign exp (op car) (reg exp))                ; exp is clause
    (test (op cond-is-arrow?) (reg exp))
    (branch (label ev-cond-arrow-actions))

    ev-cond-regular-actions
    (assign exp (op cond-actions) (reg exp))       ; exp is actions of the clause
    (assign exp (op cons) (const begin) (reg exp)) ; exp is actions prepended with "begin"
    (goto (label eval-dispatch))

    ev-cond-arrow-actions
    (assign val (op cons) (reg val) (const ())) ; val is (predicate-value)
    (assign exp (op cond-arrow-proc) (reg exp)) ; exp is the proc of the cond-arrow
    (assign exp (op cons) (reg exp) (reg val)) ; exp is (proc predicate-value)
    (goto (label eval-dispatch))

    ;;; =============

    ;;; 5.4.4 Running the Evaluator -- modified
    unknown-expression-type
      (perform (op error) (const "unknown expression type") (reg exp))
    unknown-procedure-type
      (perform (op error) (const "unknown procedure type") (reg proc))

    ;;; 5.47
    external-entry
      ; (perform (op initialize-stack))
      ; (assign env (op get-global-environment)) ; already taken care of in my code (?)
      (assign continue (label regular-eval))
      (goto (reg val))

    ;;; 5.48
    ev-compile
    (assign val (op compile-exp) (reg exp))
    (assign val (op compile) (reg val))
    (goto (reg val))

    done) m))
   m))

#| Code from book -- footnote 38 |#
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))

(define (compiled-procedure-env c-proc) (caddr c-proc))

#| 5.48 |#
(define (is-compile? exp)
  (tagged-list? exp 'compile))
(define (compile-exp exp)
  (cadr exp))

#| Tests |#

(define (eval-one exp)
  (let ([m (eval-machine)])
    (set-register-contents! m 'exp exp)
    (set-register-contents! m 'env (setup-environment))
    (set-register-contents! m 'flag #f)
    (start m)
    (get-register-contents m 'val)))
