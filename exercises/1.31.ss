#|

Exercise 1.31:

a. The "sum" procedure is only the simplest of a vast number
of similar abstractions that can be captured as higher-order
procedures. Write an analogous procedure called "product"
that returns the product of the values of a function at
points over a given range. Show how to define "factorial" in
terms of "product". Also use "product" to compute
approximations to Pi using the formula

pi   2 * 4 * 4 * 6 * 6 * 8 ...
-- = -------------------------
 4   3 * 3 * 5 * 5 * 7 * 7 ...

b. If your "product" procedure generates a recursive
process, write one that generates an iterative process. If
it generates an iterative process, write one that generates
a recursive process.

|#

#| Answer |#

(define (product term a next b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n) (* result (term n)))))
  (iter a 1))

(define (product-r term a next b)
  (if (> a b)
      1
      (* (term a) (product-r term (next a) next b))))

(define (factorial n)
  (product (lambda (x) x) 1 1+ n))

(define (pi-approx n)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (* 4 (product term 1 1+ n)))