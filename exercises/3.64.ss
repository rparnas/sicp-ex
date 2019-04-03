#|

Exercise 3.64: Write a procedure "stream-limit" that takes
as arguments a stream and a number (the tolerance). It
should examine the stream until it finds two successive
elements that differ in absolute value by less than the
tolerance, and return the second of the two elements. Using
this, we could compute square roots up to a given tolerance
by

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

|#

#| Answer |#
(load-ex "3.63")

(define (stream-limit s tolerance)
  (define (iter s)
    (let* ([head (stream-car s)]
           [tail (stream-cdr s)]
           [next (stream-car tail)])
      (if (< (abs (- head next)) tolerance)
          next
          (iter tail))))
  (iter s))

(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

#| Tests 

> (my-sqrt 55 0.001)
  7.416198487095675
; 7.416198487095662...

|#