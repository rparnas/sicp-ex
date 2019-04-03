#|

Exercise 2.8: Using reasoning analogous to Alyssa's,
describe how the difference of two intervals may be
computed. Define a corresponding subtraction procedure,
called "sub-interval".

|#

#| Answer |#

(load-ex "2.7")

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
   (- (upper-bound a) (lower-bound b))))

#| Tests |#
(define 2.8a (make-interval 7 9))
(define 2.8b (make-interval 15 20))

(define-test (sub-interval 2.8a 2.8b) '(-13 . -6))
