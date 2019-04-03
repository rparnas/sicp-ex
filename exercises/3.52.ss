#|

Exercise 3.52: Consider the sequence of expressions

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(stream-ref y 7)
(display-stream z)

What is the value of "sum" after each of the above
expressions is evaluated? What is the printed response to
evaluating the "stream-ref" and "display-stream"
expressions? Would these responses differ if we had
implemented "(delay <exp>)" simply as "(lambda () <exp>)"
without using the optimization provided by "memo-proc"?
Explain.

|#

(load-ex "3.51")

#| Code from book |#
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)

#| Answer

There are four sequences:

anonymous internal sequence counting 1 to 20.
  seq is the cumulative sum of that sequence.
    y is the even numbers from seq.
      z is numbers divisible by 5 from y.

At any time, sum is the most recently calculated and memoized value of seq.
Without memoization the values at any given index change over time because the
mapped function has side effects and the mapped function would be called
arbitrarily to recalculate parts of seq.

For example (stream-ref y 7) runs through several values of y/seq and (display-
stream z) runs through all those values again, adding them to sum again.

Seq should really be defined in a manner that avoids side effects.

> (define seq (stream-map accum (stream-enumerate-interval 1 20)))  ; 1
  ; [anon: 1...]
  ; [seq: 1...]

> (define y (stream-filter even? seq))                              ; 6
  ; [anon: 1 2 3...]
  ; [seq: 1 3 6...]
  ; [y: 6]

> (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq)) ; 10
 ; [anon: 1 2 3 4...]
 ; [seq: 1 3 6 10...]
 ; [y: 6 10...]
 ; [z: 10...]
> (stream-ref y 7)                                                  ; 136
136
> (display-stream z)                                                ; 210
10
15
45
55
105
120
190
210

|#
