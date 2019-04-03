#|

Exercise 4.27: Suppose we type in the following definitions
to the lazy evaluator:

(define count 0)
(define (id x) (set! count (+ count 1)) x)

Give the missing values in the following sequence of
interactions, and explain your answers.

(define w (id (id 10)))
 ;;; L-Eval input: 
count
 ;;; L-Eval value: 
<response>
 ;;; L-Eval input: 
w
 ;;; L-Eval value: 
<response>
 ;;; L-Eval input: 
count
 ;;; L-Eval value: 
<response>

|#

(load-ex "4.20") ;;; skipping the analyze/eval refactor from 4.22.

#| Code from book |#

;;; Re-implement eval because application? relies on being invoked last.
(define (eval exp env)
  (cond 
        ;;; 4.4 and, or
        [(is-and? exp) (eval-and (and-clauses exp) env)]
        [(is-or? exp) (eval-or (or-clauses exp) env)]
        ;;; 4.6 let
        [(is-let? exp) (eval (let->combination exp) env)]
        ;;; 4.7 let*
        [(is-let*? exp) (eval (let*->nested-lets exp) env)]
        ;;; 4.9 do-while, for
        [(is-do-while? exp) (eval (transform-do-while exp) env)]
        [(is-for? exp) (eval (transform-for exp) env)]
        ;;; 4.13 make-unbound!
        [(unassignment? exp) (eval-unassignment exp env)]
        ;;; 4.20 letrec
        [(is-letrec? exp) (eval (letrec->transform exp) env)]
        ;;; 4.1
        [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)]
        [(begin? exp)
         (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        ;;; 4.27 updated application
        [(application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env)]
        [else
         (error "eval" "unknown expression type" exp)]))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))] ; changed
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
            (procedure-parameters procedure)
            (list-of-dealyed-args arguments env) ; changed
            (procedure-environment procedure)))]
        [else
         (error "apply" "unknown procedure type" procedure)]))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-dealyed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-dealyed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

#| Code from book -- thunk |#
(define (force-it obj)
  (cond [(thunk? obj)
         (let ([result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))])
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result)]
         [(evaluated-thunk? obj)
          (thunk-value obj)]
        [else obj]))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;;; update test code
(define (eval-one exp)
  (force-it (eval exp (setup-environment))))

#| Answer 

> (define e (setup-environment))
> (eval '(define count 0) e)
> (eval '(define (id x) (set! count (+ count 1)) x) e)
> (eval '(define w (id (id 10))) e)

> (force-it (eval 'count e))
1

> (force-it (eval 'w e))
10

> (force-it (eval 'count e))
2

|#

#| Tests |#
(define-test (eval-one
  '(begin (define (try a b) (if (= a 0) 1 b))
          (try 0 (/ 1 0))))
  1)