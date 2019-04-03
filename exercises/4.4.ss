#|

Exercise 4.4: Recall the definitions of the special forms
"and" and "or" from Chapter 1:

* "and": The expressions are evaluated from left to right.
If any expression evaluates to false, false is returned; any
remaining expressions are not evaluated. If all the
expressions evaluate to true values, the value of the last
expression is returned. If there are no expressions then
true is returned.

* "or": The expressions are evaluated from left to right. If
any expression evaluates to a true value, that value is
returned; any remaining expressions are not evaluated. If
all expressions evaluate to false, or if there are no
expressions, then false is returned.

Install "and" and "or" as new special forms for the
evaluator by defining appropriate syntax procedures and
evaluation procedures "eval-and" and "eval-or".
Alternatively, show how to implement "and" and "or" as
derived expressions.

|#

(load-ex "4.1")

#| Answer |#

(define (is-and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))

(define (is-or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (eval-and clauses env)
  (define (iter last clauses)
    (if (null? clauses)
        last
        (let ([first (eval (car clauses) env)])
          (cond [(true? first) (iter first (cdr clauses))]
                [(false? first) first]
                [else (error "and" "can't be resolved to true or false" first)]))))
  (iter 'true clauses))

(define (eval-or clauses env)
  (define (iter last clauses)
    (if (null? clauses)
        last
        (let ([first (eval (car clauses) env)])
          (cond [(true? first) first]
                [(false? first) (iter first (cdr clauses))]
                [else (error "or" "can't be resolved to true or false" first)]))))
  (iter 'false clauses))

(define eval-41 eval)
(set! eval (lambda (exp env)
  (cond [(is-and? exp) (eval-and (and-clauses exp) env)]
        [(is-or? exp) (eval-or (or-clauses exp) env)]
        [else (eval-41 exp env)])))

#| A derived expressions implementation looks like |#

(define (and-transform exp)
  (define (iter clauses)
    (if (null? clauses)
        'true
        (make-if (car clauses)
                 (iter (cdr clauses))
                 'false)))
  (iter (and-clauses exp)))

(define (or-transform exp)
  (define (iter clauses)
    (if (null? clauses)
        'false
        (make-if (car clauses)
                 'true
                 (iter (cdr clauses)))))
  (iter (or-clauses exp)))

#| Testing |#
(define-test (eval-one '(and)) 'true)
(define-test (eval-one '(and true)) 'true)
(define-test (eval-one '(and true false)) 'false)
(define-test (eval-one '(and false true)) 'false)

(define-test (eval-one '(or)) 'false)
(define-test (eval-one '(or true)) 'true)
(define-test (eval-one '(or true false)) 'true)
(define-test (eval-one '(or false true)) 'true)
