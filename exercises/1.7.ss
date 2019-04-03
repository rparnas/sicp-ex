#|

Exercise 1.7: The "good-enough?" test used in computing
square roots will not be very effective for finding the
square roots of very small numbers. Also, in real computers,
arithmetic operations are almost always performed with
limited precision. This makes our test inadequate for very
large numbers. Explain these statements, with examples
showing how the test fails for small and large numbers. An
alternative strategy for implementing "good-enough?" is to
watch how "guess" changes from one iteration to the next and
to stop when the change is a very small fraction of the
guess. Design a square-root procedure that uses this kind of
end test. Does this work better for small and large numbers?

|#

#| Book code: 1.1.7 Example: Newton's method for computing square roots |#
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

#| Answer |#
(define (sqrt-alt x)
 (sqrt-iter-alt 1.0 x))

(define (sqrt-iter-alt guess x)
  (let ([next-guess (improve guess x)])
    (if (= next-guess guess) guess (sqrt-iter-alt next-guess x))))

#|

You are using a linear test to approximate an exponential
function. You will have trouble at the asymptote near zero.
For small numbers epsilon may test positive too quickly and
cause you to make too few iterations. For big numbers the
desired next guess will eventually round to the previous
guess. Alternatives could include using a constant number of
iterations or to keep going until you reach the precision of
your device.

The alternative is better for small as the system no longer
gives up too soon arbitrarily but big is not improve because
we're still running up against the precision of the device.

|#

#| Tests -- manual |#

(define small 0.00000000015413453245234554)
(define big 4871234987126413246129193847192847198274156478356913876502367401928740981237474098174087234089127340981724028734)

#|

> (sqrt small)
0.0312500016424961 ; answer is 1.2415e-5...

> (sqrt-alt small)
1.2415092929670143e-5

> (sqrt big)
6.979423319391376e55 ; answer is 8.35e27

> (sqrt-alt big)
6.979423319391376e55

|#

#| Notes

Chez Scheme's Implementation of sqrt (prim5.c):
static double s_sqrt PROTO((double x));
static double s_sqrt(x) double x; { return sqrt(x); }

How do you define goodness of the answer?

Does consistancy of numerical approximations matter?

|#