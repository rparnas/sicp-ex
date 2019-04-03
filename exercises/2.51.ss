#|

Exercise 2.51: Define the "below" operation for painters.
"below" takes two painters as arguments. The resulting
painter, given a frame, draws with the first painter in the
bottom of the frame and with the second painter in the top.
Define "below" in two different ways---first by writing a
procedure that is analogous to the "beside" procedure given
above, and again in terms of "beside" and suitable rotation
operations (from Exercise 2.50).

|#

#| Answer |#
(define (below p0 p1)
  (let ([paint-bottom (transform-painter p0
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0)
                        (make-vect 0.0 0.5))]
        [paint-top (transform-painter p1
                     (make-vect 0.0 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.0 1.0))])
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

(define (below p0 p1)
  (rotate-90
   (beside (rotate-270 p0)
      (rotate-270 p1))))
