#|

Exercise 1.21: Use the "smallest-divisor" procedure to find
the smallest divisor of each of the following numbers: 199,
1999, 19999.

|#

#| Answer |#
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
  [(divides? test-divisor n) test-divisor]
  [else (find-divisor n (+ test-divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1) (= n (smallest-divisor n))))

#| Tests |#
(define-test (smallest-divisor 199) 199)
(define-test (smallest-divisor 1999) 1999)
(define-test (smallest-divisor 19999) 7)
