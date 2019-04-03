#|

Exercise 2.49: Use "segments->painter" to define the
following primitive painters:

a. The painter that draws the outline of the designated
frame.

b. The painter that draws an "X" by connecting opposite
corners of the frame.

c. The painter that draws a diamond shape by connecting the
midpoints of the sides of the frame.

d. The "wave" painter.

|#

#|Answer|#
(load-ex "2.46")
(load-ex "2.48")

(define (segments->painter segments) 0)

(define draw-outline
  (segments->painter 
    (list (make-segment (make-vect 0 0) (make-vect 0 1))
          (make-segment (make-vect 0 1) (make-vect 1 1))
          (make-segment (make-vect 1 1) (make-vect 1 0))
          (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define draw-x
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define draw-diamond
  (segments->painter
     (list (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
           (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
           (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
           (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5)))))

(define wave 
  (segments->painter
   (list
    (make-segment (make-vect 0.25 0.00) (make-vect 0.35 0.50)) 
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60)) 
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.40)) 
    (make-segment (make-vect 0.15 0.40) (make-vect 0.00 0.65)) 
    (make-segment (make-vect 0.00 0.65) (make-vect 0.00 0.85)) 
    (make-segment (make-vect 0.00 0.85) (make-vect 0.15 0.60)) 
    (make-segment (make-vect 0.15 0.60) (make-vect 0.30 0.65)) 
    (make-segment (make-vect 0.30 0.65) (make-vect 0.40 0.65)) 
    (make-segment (make-vect 0.40 0.65) (make-vect 0.35 0.85)) 
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00)) 
    (make-segment (make-vect 0.40 1.00) (make-vect 0.60 1.00)) 
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85)) 
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65)) 
    (make-segment (make-vect 0.60 0.65) (make-vect 0.75 0.65)) 
    (make-segment (make-vect 0.75 0.65) (make-vect 1.00 0.35)) 
    (make-segment (make-vect 1.00 0.35) (make-vect 1.00 0.15)) 
    (make-segment (make-vect 1.00 0.15) (make-vect 0.60 0.45)) 
    (make-segment (make-vect 0.60 0.45) (make-vect 0.75 0.00)) 
    (make-segment (make-vect 0.75 0.00) (make-vect 0.60 0.00)) 
    (make-segment (make-vect 0.60 0.00) (make-vect 0.50 0.30)) 
    (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00)) 
    (make-segment (make-vect 0.40 0.00) (make-vect 0.25 0.00)))))
