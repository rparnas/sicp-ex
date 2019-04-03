#|

Exercise 3.72: In a similar way to Exercise 3.71 generate a
stream of all numbers that can be written as the sum of two
squares in three different ways (showing how they can be so
written).

|#

(load-ex "3.71")

#| Answer |#

(define (372-weight pair) 
  (let ([i (car pair)] [j (cadr pair)]) 
    (+ (* i i) (* j j))))

(define 372-stream
  (n-consecutive-same-weight-pairs (weighted-pairs integers integers 372-weight)
                                   3
                                   372-weight))

#| Tests |#
(define-test (map (lambda (i) (stream-ref 372-stream i)) (iota 5))
             '((325 (10 15) (6 17) (1 18))
               (425 (13 16) (8 19) (5 20))
               (650 (17 19) (11 23) (5 25))
               (725 (14 23) (10 25) (7 26))
               (845 (19 22) (13 26) (2 29))))