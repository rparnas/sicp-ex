#|

Exercise 3.55: Define a procedure "partial-sums" that takes
as argument a stream S and returns the stream whose elements
are S_0, S_0 + S_1, S_0 + S_1 + S_2,.... For example,
"(partial-sums integers)" should be the stream 1, 3, 6, 10,
15,....

|#

#| Answer 

(partial-sums i1)
p1: i1 = 1
p2: p1 + i2 = 3
p3: p2 + i3 = 6
p4: p3 + i4 = 10
...

|#

(load-ex "3.54")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))
#| Tests |#
(define (partial-sums-integers n)
  (stream-ref (partial-sums integers) (- n 1)))

(define-test (partial-sums-integers 1) 1)
(define-test (partial-sums-integers 2) 3)
(define-test (partial-sums-integers 3) 6)
(define-test (partial-sums-integers 4) 10)
(define-test (partial-sums-integers 5) 15)
