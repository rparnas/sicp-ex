#|

Exercise 3.51: In order to take a closer look at delayed
evaluation, we will use the following procedure, which
simply returns its argument after printing it:

(define (show x)
  (display-line x)
  x)

What does the interpreter print in response to evaluating
each expression in the following sequence?

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

|#

(load-ex "3.50")

#| Code from book |#
(define (show x)
  (display-line x)
  x)

#| Answer 

Show displays items in the stream as they are computed.
The first statement causes computation and memoization of element 0.
The second statement causes computation and memoization of elements 1 to 6.
The final statement causes computation and memoization of elements 7 and 8.
Note: stream-ref result returns an element causing it to be shown twice.

> (define x (stream-map show (stream-enumerate-interval 0 10)))
0 
> (stream-ref x 5)
1
2
3
4
55
> (stream-ref x 7)
6
77

|#