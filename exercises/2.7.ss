#|

Exercise 2.7: Alyssa's program is incomplete because she has
not specified the implementation of the interval
abstraction. Here is a definition of the interval
constructor:

(define (make-interval a b) (cons a b))

Define selectors "upper-bound" and "lower-bound" to complete
the implementation.

|#

#| Code from book |#
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y)) 
                                 (/ 1.0 (lower-bound y)))))

#| Answer |#
(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

#| Tests |#

; Wolfram syntax: Interval[{7,9}] + Interval[{15,20}]

(define 2.7a (make-interval 7 9))
(define 2.7b (make-interval 15 20))
(define-test (add-interval 2.7a 2.7b) '(22 . 29))
(define-test (mul-interval 2.7a 2.7b) '(105 . 180))

; > (div-interval a b)
; (0.35000000000000003 . 0.6)
