#|

Exercise 2.11: In passing, Ben also cryptically comments:
"By testing the signs of the endpoints of the intervals, it
is possible to break "mul-interval" into nine cases, only
one of which requires more than two multiplications."
Rewrite this procedure using Ben's suggestion.

After debugging her program, Alyssa shows it to a potential
user, who complains that her program solves the wrong
problem. He wants a program that can deal with numbers
represented as a center value and an additive tolerance; for
example, he wants to work with intervals such as 3.5 +/-
0.15 rather than [3.35, 3.65]. Alyssa returns to her desk
and fixes this problem by supplying an alternate constructor
and alternate selectors:

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

Unfortunately, most of Alyssa's users are engineers. Real
engineering situations usually involve measurements with
only a small uncertainty, measured as the ratio of the width
of the interval to the midpoint of the interval. Engineers
usually specify percentage tolerances on the parameters of
devices, as in the resistor specifications given earlier.

|#

#| Answer (*)

By testing the signs of the end-points of the intervals it
is possible to break mul-interval into nine cases, only one
of which requires more than two multiplcations.

|#

(load-ex "2.10")

(define (mul-interval-improved x y)
  (let* ([a (lower-bound x)]
         [b (upper-bound x)]
         [c (lower-bound y)]
         [d (upper-bound y)]
         [a1 (>= 0 a)]
         [b1 (>= 0 b)]
         [c1 (>= 0 c)]
         [d1 (>= 0 d)]
         [a0 (not a1)]
         [b0 (not b1)]
         [c0 (not c1)]
         [d0 (not d1)])
    (cond [(and a0 b1 c0 d1) (make-interval (min (* a d) (* b c)) (max (* a c) (* b d)))]
          [(and a0 b1 c0 d0) (make-interval (* b d) (* a d))]
          [(and a1 b1 c0 d0) (make-interval (* b d) (* a c))]
          [(and a0 b0 c1 d1) (make-interval (* b c) (* a c))]
          [(and a0 b0 c0 d1) (make-interval (* b d) (* b c))]
          [(and a1 b1 c0 d1) (make-interval (* b c) (* b d))]
          [else              (make-interval (* a c) (* b d))])))

#| Tests |#

(define 2.11a (make-interval 2 4))
(define 2.11b (make-interval -2 4))
(define 2.11c (make-interval -4 -2))

(define-test (mul-interval 2.11a 2.11a ) '(4 . 16))
(define-test (mul-interval 2.11a 2.11b) '(-8 . 16))
(define-test (mul-interval 2.11a 2.11c) '(-16 . -4))
(define-test (mul-interval 2.11b 2.11a ) '(-8 . 16))
(define-test (mul-interval 2.11b 2.11b) '(-8 . 16))
(define-test (mul-interval 2.11b 2.11c) '(-16 . 8))
(define-test (mul-interval 2.11c 2.11a ) '(-16 . -4))
(define-test (mul-interval 2.11c 2.11b) '(-16 . 8))
(define-test (mul-interval 2.11c 2.11c) '(4 . 16))
