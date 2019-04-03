#|

Exercise 2.1: Define a better version of "make-rat" that
handles both positive and negative arguments. "make-rat"
should normalize the sign so that if the rational number is
positive, both the numerator and denominator are positive,
and if the rational number is negative, only the numerator
is negative.

|#

#| Answer |#

(define (make-rat n d)
  (let ([g (gcd n d)]
        [sign (if (equal? (< 0 n) (< 0 d)) 1 -1)])
    (cons (/ (* sign (abs n)) g) (/ (abs d) g))))

#| Tests |#
(define-test (make-rat 1 -2) '(-1 . 2))
(define-test (make-rat -1 2) '(-1 . 2))
