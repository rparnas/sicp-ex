#|

Exercise 1.10: The following procedure computes a
mathematical function called Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

What are the values of the following expressions?

(A 1 10)
(A 2 4)
(A 3 3)

Consider the following procedures, where "A" is the
procedure defined above:

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

Give concise mathematical definitions for the functions
computed by the procedures "f", "g", and "h" for positive
integer values of n. For example, "(k n)" computes 5n^2.

|#

#| Book code |#
(define (A x y)
  (cond [(= y 0) 0]
        [(= x 0) (* 2 y)]
        [(= y 1) 2]
        [else (A (- x 1) (A x (- y 1)))]))

#| Answer |#

; A(1,10) = 2^10 or 1024
; A(2, 4) = 2^2^2^2 or 65536
; A(3, 3) = 2^2^2^2 or 65536

; f(n) = 2n
(define (f n) (A 0 n))
(define (f-guess n) (* 2 n))

; g(n) = 2^n
(define (g n) (A 1 n))
(define (g-guess n) (expt 2 n))

; h(n) = 2^(h(n-1)) when n > 1, h(0)=0, h(1)=2
(define (h n) (A 2 n))
(define (h-guess n)
  (cond [(= n 0) 0]
        [(= n 1) 2]
        [else (expt 2 (h-guess (- n 1)))]))

#| Notes

Primitive recursive: a function that can be implemented
using only do-loops. The Ackerman function is the simpliest
example of a well-defined total function that is computable
but not primitive recursive, providing a counterexample to
the belief in the early 1900s that every computable function
was also primitive recursive.

|#