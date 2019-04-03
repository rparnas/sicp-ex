#|

Exercise 2.50: Define the transformation "flip-horiz", which
flips painters horizontally, and transformations that rotate
painters counterclockwise by 180 degrees and 270 degrees.

|#

#| Code from book |#
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ([m (frame-coord-map frame)])
      (let ([new-origin (m origin)])
        (painter 
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))
#| Answer |#
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter) 
  (transform-painter painter 
                     (make-vect 0.0 1.0) 
                     (make-vect 0.0 0.0) 
                     (make-vect 1.0 1.0)))
