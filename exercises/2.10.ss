#|

Exercise 2.10: Ben Bitdiddle, an expert systems programmer,
looks over Alyssa's shoulder and comments that it is not
clear what it means to divide by an interval that spans
zero. Modify Alyssa's code to check for this condition and
to signal an error if it occurs.

|#

#| Answer |#

(load-ex "2.9")

(define (spans-zero-interval i)
  (and (<= (lower-bound i) 0) (>= (upper-bound i) 0)))

(define (div-interval a b)
  (if (spans-zero-interval b)
      (error "div-interval" "The divisor spans zero")
      (mul-interval a (make-interval (/ 1.0 (upper-bound b))
     (/ 1.0 (lower-bound b))))))

#| Tests |#
(define a (make-interval 1 16))
(define b (make-interval -5 0))

;; > (div-interval a b)
;; Exception in div-interval: The divisor spans zero
;; Type (debug) to enter the debugger.
