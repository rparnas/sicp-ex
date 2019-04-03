#|

Exercise 1.3: Define a procedure that takes three numbers as
arguments and returns the sum of the squares of the two
larger numbers.

|#

#| Answer |#

(define (sum-of-square x y)
  (+ (square x) (square y)))
 
(define (sum-of-squares-of-two-largest a b c)
  (cond [(<= a b c) (sum-of-square b c)]
        [(<= b a c) (sum-of-square a c)]
        [(<= c a b) (sum-of-square a b)]))

#| Tests |#
(define-test (sum-of-squares-of-two-largest 1 2 3) 13)
(define-test (sum-of-squares-of-two-largest 1 1 1) 2)
(define-test (sum-of-squares-of-two-largest 1 2 2) 8)
(define-test (sum-of-squares-of-two-largest 1 1 2) 5)
