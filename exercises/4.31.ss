#|

Exercise 4.31: The approach taken in this section is
somewhat unpleasant, because it makes an incompatible change
to Scheme. It might be nicer to implement lazy evaluation as
an upward-compatible extension, that is, so that ordinary
Scheme programs will work as before. We can do this by
extending the syntax of procedure declarations to let the
user control whether or not arguments are to be delayed.
While we're at it, we may as well also give the user the
choice between delaying with and without memoization. For
example, the definition

(define (f a (b lazy) c (d lazy-memo))
    ... )

would define "f" to be a procedure of four arguments, where
the first and third arguments are evaluated when the
procedure is called, the second argument is delayed, and the
fourth argument is both delayed and memoized. Thus, ordinary
procedure definitions will produce the same behavior as
ordinary Scheme, while adding the "lazy-memo" declaration to
each parameter of every compound procedure will produce the
behavior of the lazy evaluator defined in this section.
Design and implement the changes required to produce such an
extension to Scheme. You will have to implement new syntax
procedures to handle the new syntax for "define". You must
also arrange for "eval" or "apply" to determine when
arguments are to be delayed, and to force or delay arguments
accordingly, and you must arrange for forcing to memoize or
not, as appropriate.

|#

(load-ex "4.27")

#| Answer -- application |#
(define (procedure-parameter-name param)
  (if (list? param)
      (car param)
      param))

(define (procedure-parameter-type param)
  (if (list? param)
      (cadr param)
      'none))

(define (apply procedure arguments env)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))] ; same as 4.27
        [(compound-procedure? procedure)
         (let* ([p-body (procedure-body procedure)]
                [p-pars (procedure-parameters procedure)]
                [p-env (procedure-environment procedure)]
                [p-par-names (map procedure-parameter-name p-pars)])
           (eval-sequence
            p-body
            (extend-environment
              p-par-names
              (list-of-partially-delayed-args p-pars arguments env) ; changed
              p-env)))]
        [else
         (error "apply" "unknown procedure type" procedure)]))

(define (list-of-partially-delayed-args pars exps env)
  (if (no-operands? exps)
      '()
      (let* ([par (car pars)]
             [par-type (procedure-parameter-type par)]
             [exp (first-operand exps)]
             [head (cond [(eq? par-type 'none) (eval exp env)]
                         [(eq? par-type 'lazy) (lazy-it exp env)]
                         [(eq? par-type 'lazy-memo) (lazy-memo-it exp env)]
                         [else (error "apply" "bad parameter type" par-type)])])
      (cons head (list-of-partially-delayed-args (cdr pars) (rest-operands exps) env)))))

#| Answer -- thunks |#
(define (force-it obj)
  (cond [(or (lazy? obj) (lazy-memo? obj))
         (let ([result (actual-value (thunk-exp obj) (thunk-env obj))])
           (if (lazy-memo? obj)
               (begin
                 (set-car! obj 'evaluated-thunk)
                 (set-car! (cdr obj) result)
                 (set-cdr! (cdr obj) '()))
               (void))
           result)]
         [(evaluated-thunk? obj)
          (thunk-value obj)]
        [else obj]))

(define (lazy? obj) (tagged-list? obj 'lazy))
(define (lazy-it exp env) (list 'lazy exp env))

(define (lazy-memo? obj) (tagged-list? obj 'lazy-memo))
(define (lazy-memo-it exp env) (list 'lazy-memo exp env))

#| Answer -- deprecations |#
(define (delay-it . args) (error "delay-it" "deprecated"))
(define (thunk? . args) (error "thunk?" "deprecated"))
(define (list-of-dealyed-args . args) (error "list-of-dealyed-args" "deprecated"))

#| Tests

1 regression test should fail -- The test from 4.27 fails because it relies on
laziness that now must be explicitly stated. The first test below re-writes
this.

Re-test from 4.29:

  ;;; none
    > (define e (setup-environment))
    > (eval '(define count 0) e)
    > (eval '(define (id x) (set! count (+ count 1)) x) e)
    > (eval '(define (square x) (* x x)) e)
    > (force-it (eval '(square (id 10)) e))
    100
    > (force-it (eval 'count e))
    1

  ;;; lazy
    > (define e (setup-environment))
    > (eval '(define count 0) e)
    > (eval '(define (id x) (set! count (+ count 1)) x) e)
    > (eval '(define (square (x lazy)) (* x x)) e)
    > (force-it (eval '(square (id 10)) e))
    100
    > (force-it (eval 'count e))
    2

  ;;; lazy memoized
    > (define e (setup-environment))
    > (eval '(define count 0) e)
    > (eval '(define (id x) (set! count (+ count 1)) x) e)
    > (eval '(define (square (x lazy-memo)) (* x x)) e)
    > (force-it (eval '(square (id 10)) e))
    100
    > (force-it (eval 'count e))
    1
  
|#

(define-test (eval-one
  '(begin (define (try a (b lazy)) (if (= a 0) 1 b))
          (try 0 (/ 1 0))))
  1)