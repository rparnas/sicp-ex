#|

Exercise 4.7: "let*" is similar to "let", except that the
bindings of the "let*" variables are performed sequentially
from left to right, and each binding is made in an
environment in which all of the preceding bindings are
visible. For example

(let* ((x 3)  (y (+ x 2))  (z (+ x y 5)))
  (* x z))

returns 39. Explain how a "let*" expression can be rewritten
as a set of nested "let" expressions, and write a procedure
"let*->nested-lets" that performs this transformation. If we
have already implemented "let" (Exercise 4.6) and we want to
extend the evaluator to handle "let*", is it sufficient to
add a clause to "eval" whose action is

(eval (let*->nested-lets exp) env)

or must we explicitly expand "let*" in terms of non-derived
expressions?

|#

(load-ex "4.6")

#| Answer |#

(define (make-let vars exps body)
  (cons 'let
    (cons (map list vars exps)
          body)))

(define (is-let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (let*-clauses exp) (cadr exp))
  (define (let*-body exp) (cddr exp))
  (define (let*-clause-var clause) (car clause))
  (define (let*-clause-exp clause) (cadr clause))
  (let ([clauses (let*-clauses exp)]
        [body (let*-body exp)])
    (define (iter clauses)
      (let ([first (car clauses)])
        (make-let (list (let*-clause-var first))
                  (list (let*-clause-exp first))
                  (if (null? (cdr clauses))
                      body
                      (list (iter (cdr clauses)))))))
    (if (null? clauses)
        (make-begin body)
        (iter clauses))))

(define eval-46 eval)
(set! eval (lambda (exp env)
  (cond [(is-let*? exp) (eval (let*->nested-lets exp) env)]
        [else (eval-46 exp env)])))

#| Tests |#
(define-test (eval-one '(let* () 5)) 5)
(define-test (eval-one '(let* ([x 5]) x)) 5)
(define-test (eval-one '(let* ([x 5] [y (* x 2)]) y)) 10)
