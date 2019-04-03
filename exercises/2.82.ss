#|

Exercise 2.82: Show how to generalize "apply-generic" to
handle coercion in the general case of multiple arguments.
One strategy is to attempt to coerce all the arguments to
the type of the first argument, then to the type of the
second argument, and so on. Give an example of a situation
where this strategy (and likewise the two-argument version
given above) is not sufficiently general. (Hint: Consider
the case where there are some suitable mixed-type operations
present in the table that will not be tried.)

|#

(load-ex "2.81")

#| Answer 

This approach fails to coerce to any method signature which
is made up of mixed types. This could be critical for some
operations. For example, you might always want to implement
multipling a vector by a scalar as a mixed type operation.

This approach also fails to coerce if there is a valid
operation which takes arguments of a single type, but none
of your arguments happen to be that type.

|#

(define (apply-generic op . args)
  ; Call upon failure
  (define (fail) 
    (error "apply-generic" "no method for" (list op (map type-tag args))))
  ; returns args coerced to the given type or #f if impossible
  (define (coerce-args args type)
    (define (iter result args)
      (if (null? args)
          result
          (let* ([type1 (type-tag (car args))]
                 [a1 (car args)]
                 [t1->t (get-coercion type1 type)])
            (cond [(equal? type1 type)
                   (iter (append result (list a1)) (cdr args))]
                  [(not t1->t)
                   #f]
                  [else
                   (iter (append result (list (t1->t a1))) (cdr args))]))))
    (iter '() args))
  ; returns coerced versions of args to try
  (define (get-args-to-try args)
    (define (iter result types-to-try)
      (if (null? types-to-try)
          result
          (let ([coerced (coerce-args args (car types-to-try))])
            (if coerced
                (iter (append (list coerced) result)
                      (cdr types-to-try))
                (iter result 
                      (cdr types-to-try))))))
    (iter (list args) (map type-tag args)))
  ; trys to find and run a procedure against different versions of args.
  (define (try-apply-generic op args-to-try)
    (if (null? args-to-try)
        (fail)
        (let* ([args (car args-to-try)]
               [proc (get op (map type-tag args))])
          (if proc 
              (apply proc (map contents args))
              (try-apply-generic op (cdr args-to-try))))))
  ; body
  (try-apply-generic op (get-args-to-try args)))

#| Tests |#
(define (install-test-package)
  (put 'minus '(complex scheme-number)
    (lambda (a b)
      (make-complex-from-real-imag
        (sub (real-part a) b)
        (imag-part a))))
  (put 'addd '(complex complex complex)
    (lambda (x y z)
      (make-complex-from-real-imag 
        (add (real-part x) (add (real-part y) (real-part z)))
        (add (imag-part x) (add (imag-part y) (imag-part z))))))
  'done)

#|

> (install-test-package)
> (define (addd x y z) (apply-generic 'addd x y z))
> (define (minus x y) (apply-generic 'minus x y))

> (addd (make-complex-from-real-imag 1 0) 2 3)
(complex rectangular 6 . 0)

> (addd 1 (make-complex-from-real-imag 2 0) 3)
(complex rectangular 6 . 0)

> (minus (make-complex-from-real-imag 1 1) 1)
(complex rectangular 0 . 1)

; first failure case
> (minus 2 1)
Exception in apply-generic: no method for with irritant (minus (scheme-number scheme-number))
Type (debug) to enter the debugger

; second failure case
> (addd 1 2 3)
Exception in apply-generic: no method for with irritant (addd (scheme-number scheme-number scheme-number))
Type (debug) to enter the debugger.

|#