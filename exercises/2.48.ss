#|

Exercise 2.48: A directed line segment in the plane can be
represented as a pair of vectors---the vector running from
the origin to the start-point of the segment, and the vector
running from the origin to the end-point of the segment. Use
your vector representation from Exercise 2.46 to define a
representation for segments with a constructor
"make-segment" and selectors "start-segment" and
"end-segment".

|#

#| Answer |#
(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

#| Tests |#
(define s (make-segment 'start 'end))
(define-test (start-segment s) 'start)
(define-test (end-segment s) 'end)
