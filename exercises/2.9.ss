#|

Exercise 2.9: The width of an interval is half of the
difference between its upper and lower bounds. The width is
a measure of the uncertainty of the number specified by the
interval. For some arithmetic operations the width of the
result of combining two intervals is a function only of the
widths of the argument intervals, whereas for others the
width of the combination is not a function of the widths of
the argument intervals. Show that the width of the sum (or
difference) of two intervals is a function only of the
widths of the intervals being added (or subtracted). Give
examples to show that this is not true for multiplication or
division.

|#

#| Answer |#
(load-ex "2.8")

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

#| Tests |#

(define 2.9a (make-interval 1 7))
(define 2.9b (make-interval 50 86))

(define-test (width (add-interval 2.9a 2.9b)) 21)
(define-test (+ (width 2.9a) (width 2.9b)) 21)

(define-test (width (sub-interval 2.9a 2.9b)) 21)
(define-test (- (width 2.9a) (width 2.9b)) -15)

(define-test (width (mul-interval 2.9a 2.9b)) 276)
(define-test (* (width 2.9a) (width 2.9b)) 54)

; > (width (div-interval 2.9a 2.9b))
; 0.06418604651162792

; > (/ (width 2.9a) (width 2.9b))
; 1/6 ; 0.166666666..
