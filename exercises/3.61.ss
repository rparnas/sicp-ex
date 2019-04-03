#|

Exercise 3.61: Let S be a power series (Exercise 3.59) whose
constant term is 1. Suppose we want to find the power series
1 / S, that is, the series X such that SX = 1. Write S = 1 +
S_R where S_R is the part of S after the constant term. Then
we can solve for X as follows:

        S * X = 1
(1 + S_R) * X = 1
  X + S_R * X = 1
            X = 1 - S_R * X

In other words, X is the power series whose constant term is
1 and whose higher-order terms are given by the negative of
S_R times X. Use this idea to write a procedure
"invert-unit-series" that computes 1 / S for a power series
S with constant term 1. You will need to use "mul-series"
from Exercise 3.60.

|#

(load-ex "3.60")

#| Answer |#
(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s)
                                           (invert-unit-series s))
                               -1)))

#| Tests |#
(define-test (let* ([s (list->stream '(1 2 3 4))]
                    [one (mul-series s (invert-unit-series s))])
                  (list (stream-ref one 0)
                        (stream-ref one 1)
                        (stream-ref one 2)))
              (list 1 0 0))