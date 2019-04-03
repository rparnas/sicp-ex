#|

Exercise 2.85: This section mentioned a method for
"simplifying" a data object by lowering it in the tower of
types as far as possible. Design a procedure "drop" that
accomplishes this for the tower described in Exercise 2.83.
The key is to decide, in some general way, whether an object
can be lowered. For example, the complex number 1.5 + 0i can
be lowered as far as "real", the complex number 1 + 0i can
be lowered as far as "integer", and the complex number 2 +
3i cannot be lowered at all. Here is a plan for determining
whether an object can be lowered: Begin by defining a
generic operation "project" that "pushes" an object down in
the tower. For example, projecting a complex number would
involve throwing away the imaginary part. Then a number can
be dropped if, when we "project" it and "raise" the result
back to the type we started with, we end up with something
equal to what we started with. Show how to implement this
idea in detail, by writing a "drop" procedure that drops an
object as far as possible. You will need to design the
various projection operations and install "project" as a
generic operation in the system. You will also need to make
use of a generic equality predicate, such as described in
Exercise 2.79. Finally, use "drop" to rewrite
"apply-generic" from Exercise 2.84 so that it "simplifies"
its answers.

|#

(load-ex "2.84")

#| Answer |#

(define (project x) (apply-generic 'project x))

(define (upgrade-with-project)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'project '(rational) (lambda (x)
    (make-integer (round (/ (numer x) (denom x))))))
  (put 'project '(real) (lambda (x)
    (let ([frac (inexact->exact x)])
      (make-rational (numerator frac)
                     (denominator frac)))))
  (put 'project '(complex) (lambda (x) 
    (make-real (real-part x))))
  'done)

(upgrade-with-project)

(define (drop x)
  (if (or (not (pair? x)) (not (get 'project (list (type-tag x)))))
      x
      (let ([projection (project x)])
        (if (equ? x projection)
            (drop projection)
            x))))

(define old-apply-generic apply-generic)
(define (apply-generic op . args)
  (drop (apply old-apply-generic (append (list op) args))))

#| Tests -- expect 1 regression failure due to new drop feature |#

(define-test (drop (make-integer 1))
             '(integer . 1))
(define-test (drop (make-rational 1 1))
             '(integer . 1))
(define-test (drop (make-real 1.0))
             '(integer . 1))
(define-test (drop (make-complex-from-real-imag 1.0 0.0))
             '(integer . 1))

(define-test (drop (make-complex-from-real-imag 2.0 2.0))
             '(complex rectangular 2.0 . 2.0))
(define-test (drop (make-rational 1 2))
             '(rational 1 . 2))
(define-test (drop (make-real (/ 1 3)))
            '(rational 1 . 3))

(define-test (add (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
             '(complex rectangular 2 . 4))
(define-test (add (make-complex-from-real-imag 1 0) (make-complex-from-real-imag 1 0))
             '(integer . 2))

#| Notes 

It is weird that constructors bypass apply-generic's new
autosimplification functionality but selectors do not. It is
also kind of weird that selectors don't give you back what
you put in.

This is probably beyond scope because the underlying scheme
number system doesn't have a way to represent an irrational
real number. For example (sqrt 2) yields 1.4142135623730951
which our type system might simplify to a really long
fraction but an irrational number is what was actually
meant.

Perhaps the only solution would be to add types for various
mathematical operators such as sqrt. In effect, complex is a
type meaning f(x, y) = x + iy so you could have another type
which is f(x) = sqrt(x).

|# 
