#|

Exercise 2.46: A two-dimensional vector v running from the
origin to a point can be represented as a pair consisting of
an x-coordinate and a y-coordinate. Implement a data
abstraction for vectors by giving a constructor "make-vect"
and corresponding selectors "xcor-vect" and "ycor-vect". In
terms of your selectors and constructor, implement
procedures "add-vect", "sub-vect", and "scale-vect" that
perform the operations vector addition, vector subtraction,
and multiplying a vector by a scalar:

(x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)
(x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)
             s * (x, y) = (sx, sy)

|#

#| Answer |#
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v0 v1)
  (make-vect (+ (xcor-vect v0) (xcor-vect v1))
             (+ (ycor-vect v0) (ycor-vect v1))))

(define (sub-vect v0 v1)
  (make-vect (- (xcor-vect v0) (xcor-vect v1))
             (- (ycor-vect v0) (ycor-vect v1))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

#| Tests |#
(define a (make-vect 4 5))
(define b (make-vect 9 4))
(define-test (add-vect a b) '(13 . 9))
(define-test (sub-vect a b) '(-5 . 1))
(define-test (scale-vect a 3) '(12 . 15))
