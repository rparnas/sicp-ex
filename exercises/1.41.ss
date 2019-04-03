#|

Exercise 1.41: Define a procedure "double" that takes a
procedure of one argument as argument and returns a
procedure that applies the original procedure twice. For
example, if "inc" is a procedure that adds 1 to its
argument, then "(double inc)" should be a procedure that
adds 2. What value is returned by

(((double (double double)) inc) 5)

|#

#| Code from book |#
(define (inc x) (+ 1 x))

#| Answer |#

(define (double f)
  (lambda (x) (f (f x))))

#| Tests |#
; Like (((double apply-double-twice) inc) 5) or [5 + (2^4)] or [5 + 16].
(define-test (((double (double double)) inc) 5) 21)
