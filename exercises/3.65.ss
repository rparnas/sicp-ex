#|

Exercise 3.65: Use the series

            1     1     1
ln 2 = 1 - --- + --- - --- + ...
            2     3     4

to compute three sequences of approximations to the natural
logarithm of 2, in the same way we did above for Pi. How
rapidly do these sequences converge?

|#

(load-ex "3.64")

#| Code from book |#

;;; pi/4 = 1 - 1/3 + 1/5 - 1/7...
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
        [s2 (stream-ref s 2)])
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

#| Answer 

2000, 7 and 4 terms respectively.

> (define t .0005)

> (counted-stream-limit ln-of-2-stream t)
(0.6928972430599403 . 2000)

> (counted-stream-limit (euler-transform ln-of-2-stream) t) 
(0.6933473389355742 . 7)

> (counted-stream-limit (accelerated-sequence euler-transform ln-of-2-stream) t)
(0.6931488693329254 . 4)

|#

;;; returns (result . number-of-indicies-evaluated)
(define (counted-stream-limit s tolerance)
  (define (iter s n)
    (let* ([head (stream-car s)]
           [tail (stream-cdr s)]
           [next (stream-car tail)])
    (if (< (abs (- head next)) tolerance)
        (cons next n)
        (iter tail (+ n 1)))))
  (iter s 2))

(define (ln-of-2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-of-2-summands (+ n 1)))))

(define ln-of-2-stream
  (partial-sums (ln-of-2-summands 1)))