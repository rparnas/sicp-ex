#|

Exercise 2.12: Define a constructor "make-center-percent"
that takes a center and a percentage tolerance and produces
the desired interval. You must also define a selector
"percent" that produces the percentage tolerance for a given
interval. The "center" selector is the same as the one shown
above.

|#

#| Answer |#
(load-ex "2.11")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (abs (* c (/ p 100)))))
(define (percent i)
  (* 100 (/ (width i) (center i))))

#| Tests |#
(define-test (make-center-percent 10 10) '(9 . 11))
(define-test (percent (make-center-percent 10 10)) 10)
