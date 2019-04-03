#|

Exercise 2.5: Show that we can represent pairs of
nonnegative integers using only numbers and arithmetic
operations if we represent the pair a and b as the integer
that is the product 2^a 3^b. Give the corresponding
definitions of the procedures "cons", "car", and "cdr".

|#

#| Answer

Seems like prime factorization

|#

(define (pull-factor x f)
  (define (iter result x)
    (if (not (= (remainder x f) 0))
 result
 (iter (+ result 1) (/ x f))))
  (iter 0 x))

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car p)
  (pull-factor p 2))

(define (cdr p)
  (pull-factor p 3))

#| Tests -- manual

> (cons 7 9)
2519424

> (car (cons 7 9))
7

> (cdr (cons 7 9))
9

|#