#|

Exercise 4.13: Scheme allows us to create new bindings for
variables by means of "define", but provides no way to get
rid of bindings. Implement for the evaluator a special form
"make-unbound!" that removes the binding of a given symbol
from the environment in which the "make-unbound!" expression
is evaluated. This problem is not completely specified. For
example, should we remove only the binding in the first
frame of the environment? Complete the specification and
justify any choices you make.

|#

(load-ex "4.12")

#| Answer 

You can access a variable on any frame in the environment so I will allow the
user to undefine any variable in the environment.

|#

(define (remove-binding-from-frame! var frame)
  (set-cdr! frame (filter (lambda (pair) (not (eq? var (car pair)))) (cdr frame))))

(define (unassignment? exp) (tagged-list? exp 'make-unbound!))
(define (unassigment-variable exp) (cadr exp))
(define (unassignment-value exp) (caddr exp))
(define (eval-unassignment exp env)
  (undefine-variable! (definition-variable exp) env)
  (void))

(define (undefine-variable! var env)
  (let ([frame (find-frame-with-binding var env)])
    (if frame
        (remove-binding-from-frame! var frame)
        (error "" "unbound-variable" var))))

(define eval-412 eval)
(set! eval (lambda (exp env)
  (cond [(unassignment? exp) (eval-unassignment exp env)]
        [else (eval-412 exp env)])))

#| Tests 

> (define env (setup-environment))
> (eval '(define x 5) env)
ok
> (eval 'x env)
5
> (eval '(make-unbound! x) env)
> (eval 'x env)
Exception: unbound-variable with irritant x
Type (debug) to enter the debugger.

|#

#| Notes

This is odd because it allows you to explore the environment by unshadowing a
variable. It needs to be investigated what pros and cons unshadowing has.

For additional iterations, I'd consider:

(1) Create a seperate "explore the environment API"

(2) Avoid unshadowing with a special "undefined" value and a primitive "is-
undefined?" predicate. If you undefine something I'd set it to this value rather
than removing its binding from the environment. Also variable lookups would
never return an error, each procedure would decide whether to handle undefined
or not.

|#