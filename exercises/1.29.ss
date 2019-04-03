#|

Exercise 1.29: Simpson's Rule is a more accurate method of
numerical integration than the method illustrated above.
Using Simpson's Rule, the integral of a function f between a
and b is approximated as

h
- (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n)
3

where h = (b - a) / n, for some even integer n, and y_k =
f(a + kh). (Increasing n increases the accuracy of the
approximation.) Define a procedure that takes as arguments
f, a, b, and n and returns the value of the integral,
computed using Simpson's Rule. Use your procedure to
integrate "cube" between 0 and 1 (with n = 100 and n =
1000), and compare the results to those of the "integral"
procedure shown above.

|#

#| Answer |#
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k) (cond [(or (= k 0) (= k n)) (y k)]
       [(odd? k) (* 4 (y k))]
       [else (* 2 (y k))]))
  (* (/ h 3) (sum term 0 (lambda (x) (+ 1 x)) n)))

#| Tests -- manual

> (exact->inexact (simpson (lambda (x) (/ 1 x)) 1 16 16) )
2.783201164931861 ; approximation of log_2(16) or 4

> (simpson cube 0 1 16)
1/4 ; answer is 1/4

|#