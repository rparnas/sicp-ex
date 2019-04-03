#|

Exercise 4.20: Because internal definitions look sequential
but are actually simultaneous, some people prefer to avoid
them entirely, and use the special form "letrec" instead.
"letrec" looks like "let", so it is not surprising that the
variables it binds are bound simultaneously and have the
same scope as each other. The sample procedure "f" above can
be written without internal definitions, but with exactly
the same meaning, as

(define (f x)
  (letrec
    ((even? (lambda (n)
              (if (= n 0) true  (odd?  (- n 1)))))
     (odd?  (lambda (n)
              (if (= n 0) false (even? (- n 1))))))
    <rest of body of "f">))

"letrec" expressions, which have the form

(letrec ((<var_(1)>  <exp_(1)> )   ...  (<var_(n)>  <exp_(n)> ))
  <body>)

are a variation on "let" in which the expressions <exp_k>
that provide the initial values for the variables <var_k>
are evaluated in an environment that includes all the
"letrec" bindings. This permits recursion in the bindings,
such as the mutual recursion of "even?" and "odd?" in the
example above, or the evaluation of 10 factorial with

(letrec
  ((fact (lambda (n)
           (if (= n 1) 1 (* n (fact (- n 1)))))))
  (fact 10))

a. Implement "letrec" as a derived expression, by
transforming a "letrec" expression into a "let" expression
as shown in the text above or in Exercise 4.18. That is, the
"letrec" variables should be created with a "let" and then
be assigned their values with "set!".

b. Louis Reasoner is confused by all this fuss about
internal definitions. The way he sees it, if you don't like
to use "define" inside a procedure, you can just use "let".
Illustrate what is loose about his reasoning by drawing an
environment diagram that shows the environment in which the
<rest of body of "f"> is evaluated during evaluation of the
expression "(f 5)", with "f" defined as in this exercise.
Draw an environment diagram for the same evaluation, but
with "let" in place of "letrec" in the definition of "f".

|#

(load-ex "4.17")

#| Answer 

a. see below

b. let-frame below is the current frame at <rest of body of "f">

   global[(f *)]
         ^
         |               
  f-frame[(x 5)___________________________________________]
         ^
         |
         |               (odd? (n) (...even?...) (env))
         |                ^                      |
         |                |                      V
let-frame[(even? *) (odd? *)______________________________]
                 |                                        ^
                 V                                        |
                (even? (n) (...odd?...) _________________ (env))

If an ordinary let were used, the two procedures are created in the context of
f-frame and thus would not have a reference to themselves or each other, causing
their bodies to fail.

   global[(f *)]
         ^
         |               
  f-frame[(x 5)________________________________________________]
         ^                                       ^          ^
         |                                       |          |
         |               (odd? (n) (...even?...) (env))     |
         |                ^                                 |
         |                |                                 |
let-frame[(even? *) (odd? *)______________________________] |
                 |                                          |
                 V                                          |
                (even? (n) (...odd?...) _________________ (env))

|#

(define (is-letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->transform exp)
  (define (letrec-clauses exp) (cadr exp))
  (define (letrec-body exp) (cddr exp))
  (define (letrec-clause-var clause) (car clause))
  (define (letrec-clause-exp clause) (cadr clause))
  (let ([clauses (letrec-clauses exp)]
        [body (letrec-body exp)])
    (make-let (map letrec-clause-var clauses)
              (map (lambda (clause) ''*undefined*) clauses)
              (append (map (lambda (clause)
                             (make-assignment (letrec-clause-var clause)
                                              (letrec-clause-exp clause)))
                           clauses)
                      body))))

(define eval-417 eval)
(set! eval (lambda (exp env)
  (cond [(is-letrec? exp) (eval (letrec->transform exp) env)]
        [else (eval-417 exp env)])))

#| Tests |#
(define-test (eval-one '(begin 
                          (define (f x)
                            (letrec 
                              ([even? (lambda (n) (if (= n 0) true (odd? (- n 1))))]
                               [odd? (lambda (n) (if (= n 0) false (even? (- n 1))))])
                              (cons (even? x) (odd? x))))
                          (f 7)))
              '(false . true))
