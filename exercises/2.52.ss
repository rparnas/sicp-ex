#|

Exercise 2.52: Make changes to the square limit of "wave"
shown in Figure 2.9 by working at each of the levels
described above. In particular:

a. Add some segments to the primitive "wave" painter of
Exercise 2.49 (to add a smile, for example).

b. Change the pattern constructed by "corner-split" (for
example, by using only one copy of the "up-split" and
"right-split" images instead of two).

c. Modify the version of "square-limit" that uses
"square-of-four" so as to assemble the corners in a
different pattern. (For example, you might make the big Mr.
Rogers look outward from each corner of the square.)

|#

(define smiling-wave
  (lambda (frame)
    (wave frame)
    ((segments->painter
      (make-segment ; eye
       (make-vect 0.395 0.916)
       (make-vect 0.410 0.916))
      (make-segment ; smile
       (make-vect 0.376 0.746)
       (make-vect 0.460 0.790)))
    frame)))

(define (corner-split painter n)
 (if (= n 0)
     painter
     (let ((up (up-split painter (- n 1)))
           (right (right-split painter (- n 1)))
           (corner (corner-split painter (- n 1))))
       (beside (below painter up)
               (below right corner)))))

(define (square-limit painter n)
 (let ((quarter (rotate180 (corner-split painter n))))
  (let ((half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half))))
