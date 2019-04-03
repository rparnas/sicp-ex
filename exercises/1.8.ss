#|

Exercise 1.8: Newton's method for cube roots is based on the
fact that if y is an approximation to the cube root of x,
then a better approximation is given by the value

x/y^2 + 2y
----------
    3

Use this formula to implement a cube-root procedure
analogous to the square-root procedure. (In Section 1.3.4 we
will see how to implement Newton's method in general as an
abstraction of these square-root and cube-root procedures.)

|#

#| Answer |#

(define (cube x)
  (* x x x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (improve guess x)
  (define y guess)
    (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cbrt x)
  (cbrt-iter 1.0 x))

#| Tests -- manual

> (cbrt 7)
1.9129320405969417 ; answer is 1.91293118277...

|#
