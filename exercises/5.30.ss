#|

Exercise 5.30: Our evaluator currently catches and signals
only two kinds of errors---unknown expression types and
unknown procedure types. Other errors will take us out of
the evaluator read-eval-print loop. When we run the
evaluator using the register-machine simulator, these errors
are caught by the underlying Scheme system. This is
analogous to the computer crashing when a user program makes
an error. It is a large project to make a real error system
work, but it is well worth the effort to understand what is
involved here.

a. Errors that occur in the evaluation process, such as an
attempt to access an unbound variable, could be caught by
changing the lookup operation to make it return a
distinguished condition code, which cannot be a possible
value of any user variable. The evaluator can test for this
condition code and then do what is necessary to go to
"signal-error". Find all of the places in the evaluator
where such a change is necessary and fix them. This is lots
of work.

b. Much worse is the problem of handling errors that are
signaled by applying primitive procedures, such as an
attempt to divide by zero or an attempt to extract the "car"
of a symbol. In a professionally written high-quality
system, each primitive application is checked for safety as
part of the primitive. For example, every call to "car"
could first check that the argument is a pair. If the
argument is not a pair, the application would return a
distinguished condition code to the evaluator, which would
then report the failure. We could arrange for this in our
register-machine simulator by making each primitive
procedure check for applicability and returning an
appropriate distinguished condition code on failure. Then
the "primitive-apply" code in the evaluator can check for
the condition code and go to "signal-error" if necessary.
Build this structure and make it work. This is a major
project.

|#

(load-ex "5.22")
(load-ex "4.7")

#| Answer

  The evaluator machine is a version of the metacircular evaluator, partially
  compiled to the register machine language by hand. Firstly, I edited the
  metacircular evaluator with the new error features in scheme, ignoring machine
  language entirely.

  The evaluator is the one from 4.1, patched by 4.4, 4.5, 4.6, 4.7. The first
  modification is to change lookup-variable-value and set-variable-value! to
  sometimes return an error signal. To do this trace everywhere these functions
  are called from the top eval call and make sure nothing in between operate on
  either lookup-variable-value or set-variable-value! without checking if the
  returned value is an an error signal.

  lookup-variable-value is only called by eval, so now check calls to eval is called by:
  * 4.1
    * eval-assignment -- tail-called by eval: added check in eval-assignment
    * eval-definition -- tail-called by eval: added check in eval-definition
    * eval-if -- called by eval: added check for predicate only, consequent/alternative are tail-calls
    * eval-sequence -- tail-called by eval: added check in eval-sequence
                       tail-called by itself: can ignore
                       tail-called by apply for a proc body, (apply is only tail-called by eval): can ignore
    * list-of-values -- called in the apply case of eval: added check to apply case AND added in list-of-values
                     -- called by itself: added check
  * 4.4
    * eval-and -- tail-called by eval: added check in eval-and
    * eval-or -- tail-called by eval: added check in eval-or

  and inside eval itself:
  * 4.1 -- cond case -- simple syntactic transformation: can ignore
  * 4.1 -- application case: added check in case itself
  * 4.6 -- let case -- simple syntactic transformation: can ignore
  * 4.7 -- let* case -- simple syntactic transformation: can ignore

  set-variable-value! is only called inside eval-assignment: added a check

  The second modification is that primitive procedures should signal logical
  errors. Primitive procedures are invoked by apply-primitive-procedure which
  is tail-called only by apply, which is tail-called only by eval. So as
  long as the primitive procedure itself returns a signal, everything will be
  fine because we've already checked that everything that calls eval does the
  correct signal checking. As an example, I'll only add this new functionality
  to division, for the divide by zero case.
|#

;;; for now just use something which can be a user value.
(define (make-condition-code code irritant) (list 'condition code irritant))
(define (is-condition? exp) (tagged-list? exp 'condition))

;;; rewrite from 4.1 ~ 4.7
(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env)]
        [(begin? exp) (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        [(is-and? exp) (eval-and (and-clauses exp) env)] ; 4.4
        [(is-or? exp) (eval-or (or-clauses exp) env)] ; 4.4
        [(is-let? exp) (eval (let->combination exp) env)] ; 4.6
        [(is-let*? exp) (eval (let*->nested-lets exp) env)] ; 4.7
        [(application? exp)
         (let ([op (eval (operator exp) env)])
           (if (is-condition? op) ; new 
               op
               (let ([lov (list-of-values (operands exp) env)])
                 (if (is-condition? lov) ; new
                     lov
                     (apply op lov)))))]
        [else (error "eval" "unknown expression type" exp)]))

;;; modification from 4.1
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (car vals)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (make-condition-code 'unbound-variable var) ; modification
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; modification from 4.1
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (make-condition-code 'unbound-variable var) ; modification
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; modification from 4.1
(define (eval-assignment exp env)
  (let ([value (eval (assignment-value exp) env)])
    (if (is-condition? value) ; new
        value
        (begin 
          (let ([result (set-variable-value! (assignment-variable exp) value env)])
            (if (is-condition? result) ; new 
                result
                'ok))))))

;;; modification from 4.1
(define (eval-definition exp env)
    (let ([value (eval (definition-value exp) env)])
      (if (is-condition? value) ; new
          value
          (begin 
            (define-variable! (definition-variable exp)
                              value
                              env)
            'ok))))

;;; modification from 4.1
(define (eval-if exp env)
  (let ([p (eval (if-predicate exp) env)])
    (if (is-condition? p) ; new
        p
        (if (true? p)
            (eval (if-consequent exp) env)
            (eval (if-alternative exp) env)))))

;;; modification from 4.1
(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (let ([first (eval (first-exp exps) env)])
                (if (is-condition? first) ; new
                    first
                    (eval-sequence (rest-exps exps) env)))]))

;;; modification from 4.1
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ([first (eval (first-operand exps) env)])
        (if (is-condition? first) ; new 
            first
            (let ([rest (list-of-values (rest-operands exps) env)])
              (if (is-condition? rest) ; new
                  rest
                  (cons first rest)))))))

;;; modification from 4.4
(define (eval-and clauses env)
  (define (iter last clauses)
    (if (null? clauses)
        last
        (let ([first (eval (car clauses) env)])
          (cond [(is-condition? first) first] ; new
                [(true? first) (iter first (cdr clauses))]
                [(false? first) first]
                [else (error "and" "can't be resolved to true or false" first)]))))
  (iter 'true clauses))

;;; modification from 4.4
(define (eval-or clauses env)
  (define (iter last clauses)
    (if (null? clauses)
        last
        (let ([first (eval (car clauses) env)])
          (cond [(is-condition? first) first] ; new
                [(true? first) first]
                [(false? first) (iter first (cdr clauses))]
                [else (error "or" "can't be resolved to true or false" first)]))))
  (iter 'false clauses))

;;; modification from 4.1 -- breaks original division which allows more than 1 or more args.
(set-cdr! (assoc '/ primitive-procedures)
          (list (lambda (a b)
                  (if (= b 0)
                      (make-condition-code 'exception "divide by zero")
                      (/ a b)))))

#| Answer (con't) 

The evaluator returns signals instead of relying upon the underlying scheme to
deal with exceptions. Port the changes to the register machine.

The starting point of the register machine evaluator is a copy-paste from 5.23.

The following functions were modified in the metacircular evaluator:
  * eval: added checks to ev-apply, etc.
  * lookup-variable-value: primitive, skip
  * set-variable-value!: primitive, skip
  * eval-assignment: added checks to ev-assignment
  * eval-definition: added to ev-definition
  * eval-if: added to ev-if
  * eval-sequence: added to ev-sequence
  * list-of-values: taken care of as part of ev-apply, etc.
  * eval-and: primitive, skip
  * eval-or: primitive, skip

In a full-fledged register machine system I imagine you might use some kind of
standard error singaling (like a flag register) and also do things like clear
the stack and be able to catch errors.

In a full-fledged metacirculator evaluator system you'd probably want to
implement things using some sort of an error continuation or exception system.
Right now things are over-coupled because procedures must know what types of
errors the procedures they call will signal.

|#

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

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
    (list 'is-condition? is-condition?))
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
      (test (op is-condition?) (reg val)) ; new
      (branch (label signal-error))
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
      (test (op is-condition?) (reg val)) ; new -- not last arg failed
      (branch (label signal-error))
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
      (test (op is-condition?) (reg val)) ; new -- last arg failed
      (branch (label signal-error))
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
      (assign exp (op first-exp) (reg unev))
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (test (op is-condition?) (reg val)) ; new
      (branch (label signal-error))
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
      (test (op is-condition?) (reg val))
      (branch (label signal-error))
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
      (test (op is-condition?) (reg val)) ; new
      (branch (label signal-error))
      (restore continue)
      (restore env)
      (restore unev)
      (assign val (op set-variable-value!) (reg unev) (reg val) (reg env))
      (test (op is-condition?) (reg val)) ; new
      (branch (label signal-error))
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
      (test (op is-condition?) (reg val)) ; new
      (branch (label signal-error))
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
    signal-error)))

#| Tests 

  1 regression test fails -- cond arrow syntax

|#

;;; re-wire eval-one such that it runs both the metacircular-evaluator directly
;;; and the register machine language evaluator and compares the answers between
;;; them, throwing an error if the results don't match.

(define eval-one-metacircular eval-one)
(define (eval-one-register-machine exp)
  (let ([m (eval-machine)])
    (set-register-contents! m 'exp exp)
    (set-register-contents! m 'env (setup-environment))
    (start m)
    (get-register-contents m 'val)))
(define (eval-one exp)
  (let ([meta-result (eval-one-metacircular exp)]
        [reg-result (eval-one-register-machine exp)])
    (if (equal? meta-result reg-result)
        meta-result
        "results don't match!")))

(define-test (eval-one '(unk 2 2))
             '(condition unbound-variable unk))
(define-test (eval-one '(and 2 unk))
             '(condition unbound-variable unk))
(define-test (eval-one '(or false unk))
             '(condition unbound-variable unk))
(define-test (eval-one '(begin (define a 0) (set! a unk)))
             '(condition unbound-variable unk))
(define-test (eval-one '(define a unk))
             '(condition unbound-variable unk))
(define-test (eval-one '(begin 5 unk))
             '(condition unbound-variable unk))
(define-test (eval-one '(begin unk 5))
             '(condition unbound-variable unk))
(define-test (eval-one '(begin (define (x) (+ 2 2) (unk 2 2)) (x)))
             '(condition unbound-variable unk))
(define-test (eval-one '(+ unk 2))
             '(condition unbound-variable unk))
(define-test (eval-one '(+ 2 unk))
             '(condition unbound-variable unk))
(define-test (eval-one '(set! unk 0))
             '(condition unbound-variable unk))
(define-test (eval-one '(/ 5 0))
             '(condition exception "divide by zero"))
(define-test (eval-one '(if unk 0 1))
            '(condition unbound-variable unk))
(define-test (eval-one '(if true unk 1))
            '(condition unbound-variable unk))
(define-test (eval-one '(if false 1 unk))
            '(condition unbound-variable unk))