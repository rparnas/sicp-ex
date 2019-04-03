#|

Exercise 1.40: Define a procedure "cubic" that can be used
together with the "newtons-method" procedure in expressions
of the form

(newtons-method (cubic a b c) 1)

to approximate zeros of the cubic x^3 + ax^2 + bx + c.

|#

#| Code from 1.3.4 |#
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
      dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

#| Answer |#
(load-ex "1.36")

(define (cubic a b c)
  (lambda (x)
    (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

#| Tests 

; x^3 + ax^2 + bx + c
; a=5, b=2, c=7
; x^3 + 5x^2 + 2x + 7
> (newtons-method (cubic 5 2 7) 1)
-4.883959583128609 ; answer is ~= -4.8840

|#