#|

Exercise 1.45: We saw in Section 1.3.3 that attempting to
compute square roots by naively finding a fixed point of y
|--> x / y does not converge, and that this can be fixed by
average damping. The same method works for finding cube
roots as fixed points of the average-damped y |--> x / y^2.
Unfortunately, the process does not work for fourth
roots---a single average damp is not enough to make a
fixed-point search for y |--> x / y^3 converge. On the other
hand, if we average damp twice (i.e., use the average damp
of the average damp of y |--> x / y^3) the fixed-point
search does converge. Do some experiments to determine how
many average damps are required to compute n^(th) roots as a
fixed-point search based upon repeated average damping of y
|--> x / y^(n-1). Use this to implement a simple procedure
for computing n^(th) roots using "fixed-point",
"average-damp", and the "repeated" procedure of Exercise
1.43. Assume that any arithmetic operations you need are
available as primitives.

|#

#| Answer |#

(load-ex "1.36")
(load-ex "1.43")

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nr-test x n damps)
  (fixed-point
   ((repeated average-damp damps) (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

#|

Running test in the format (nr-test 50 n damps) to see how
many damps are needed until the function terminates.

n={2, 3} --> 1 damp
n={4, 5, 6, 7} --> 2 damp
n={8, 9, 10, 11, 12, 13, 14, 15} --> 3 damp

Seems like (floor (log2 n)) damps are needed.

|#

(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (floor (log n 2))) (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

#| Tests -- manual

> (map (lambda (n) (nth-root 50 n)) '(2 4 6 8 10 12 100 200 1000))
(7.071067811865477 
 2.6591479484724947 
 1.919385294102911 
 1.6306894089657187
 1.4787565109488354 
 1.3854155762917304 
 1.0398920482147518
 1.0197554540946159 
 1.003917261685058))

 |#
