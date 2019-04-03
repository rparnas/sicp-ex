#|

Exercise 2.14: Demonstrate that Lem is right. Investigate
the behavior of the system on a variety of arithmetic
expressions. Make some intervals A and B, and use them in
computing the expressions A / A and A / B. You will get the
most insight by using intervals whose width is a small
percentage of the center value. Examine the results of the
computation in center-percent form (see Exercise 2.12).

|#

#| Code From Book |#
(load-ex "2.12")

(define (par1 r1 r2)
   (div-interval (mul-interval r1 r2)
                 (add-interval r1 r2)))

(define (par2 r1 r2)
   (let ((one (make-interval 1 1)))
      (div-interval one
                    (add-interval (div-interval one r1)
                                  (div-interval one r2)))))

#| Tests |#

(define 2.14a (make-center-percent 60 5))
(define 2.14b (make-center-percent 75 4))

; > (par1 2.11c 2.14b)
; (29.106382978723403 . 38.093023255813954)

; > (par2 2.11c 2.14b)
; (31.813953488372096 . 34.851063829787236)
