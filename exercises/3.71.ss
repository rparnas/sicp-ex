#|

Exercise 3.71: Numbers that can be expressed as the sum of
two cubes in more than one way are sometimes called
Ramanujan numbers, in honor of the mathematician Srinivasa
Ramanujan. Ordered streams of pairs provide an elegant
solution to the problem of computing these numbers. To find
a number that can be written as the sum of two cubes in two
different ways, we need only generate the stream of pairs of
integers (i, j) weighted according to the sum i^3 + j^3 (see
Exercise 3.70), then search the stream for two consecutive
pairs with the same weight. Write a procedure to generate
the Ramanujan numbers. The first such number is 1,729. What
are the next five?

|#

(load-ex "3.70")

#| Answer |#
(define (ramanujan-weight pair)
  (let ([i (car pair)] [j (cadr pair)])
    (+ (expt i 3) (expt j 3))))

(define (n-consecutive-same-weight-pairs s n W)
  (let* ([cars (map (lambda (i) (stream-ref s i)) (iota n))]
         [weights (map W cars)])
    (if (andmap (lambda (x) (= (car weights) x)) weights)
        (cons-stream (cons (car weights) cars)
                     (n-consecutive-same-weight-pairs (stream-cdr s) n W))
        (n-consecutive-same-weight-pairs (stream-cdr s) n W))))

(define ramanujan-stream
  (n-consecutive-same-weight-pairs (weighted-pairs integers integers ramanujan-weight)
                                   2
                                   ramanujan-weight))

#| Tests |#

;;; checked first 6 ramanujan numbers against reference.
(define-test (map (lambda (i) (stream-ref ramanujan-stream i)) (iota 6))
             '((1729 (9 10) (1 12)) 
               (4104 (9 15) (2 16)) 
               (13832 (18 20) (2 24))
               (20683 (19 24) (10 27)) 
               (32832 (18 30) (4 32))
               (39312 (15 33) (2 34))))