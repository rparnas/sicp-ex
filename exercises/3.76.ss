#|

Exercise 3.76: Eva Lu Ator has a criticism of Louis's
approach in Exercise 3.75. The program he wrote is not
modular, because it intermixes the operation of smoothing
with the zero-crossing extraction. For example, the
extractor should not have to be changed if Alyssa finds a
better way to condition her input signal. Help Louis by
writing a procedure "smooth" that takes a stream as input
and produces a stream in which each element is the average
of two successive input stream elements. Then use "smooth"
as a component to implement the zero-crossing detector in a
more modular style.

|#

(load-ex "3.75")

#| Answer |#
(define (smooth s)
  (define (smooth-inner input-stream one-back-value)
      (let* ([this-value (stream-car input-stream)]
             [this-avg (/ (+ this-value one-back-value) 2)])
        (cons-stream this-avg 
                     (smooth-inner (stream-cdr input-stream) 
                                   this-value))))
  (smooth-inner s 0))

;;; use technique from 3.74
(define smoothed-zero-crossings-376
  (let ([smoothed (smooth sense-data)])
    (stream-map sign-change-detector smoothed (cons-stream 0 smoothed))))

#| Tests |#

;;; should be the same as 3.75
(define-test (map (lambda (i) (stream-ref smoothed-zero-crossings-376 i)) (iota 13))
             '(0 0 0 0 0  0 -1 0 0 0 0 1 0))

#| Notes 

Perhaps a better smooth procedure would (1) take into account how to deal with
the begining of the stream and (2) would take a procedure like map, etc. does.

|#
