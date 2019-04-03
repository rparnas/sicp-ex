#|

Exercise 3.67: Modify the "pairs" procedure so that "(pairs
integers integers)" will produce the stream of all pairs of
integers (i, j) (without the condition i <= j). Hint: You
will need to mix in an additional stream.

|#

#| Answer |#
(load-ex "3.66")

(define (all-pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (interleave
        (stream-map (lambda (x) (list x (stream-car s))) (stream-cdr t))
        (pairs (stream-cdr s) (stream-cdr t))))))

#| Notes 

(1 1) (1 2) (1 3) (1 4)...
(2 1) (2 2) (2 3) (2 4)...
(3 1) (3 2) (3 3) (3 4)...
(4 1) (4 2) (4 3) (4 4)...

at (1 1)...
  the rest of the row is (1 2) (1 3) (1 4)...
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
  the rest of the col is (2 1) (3 1) (4 1)...
    (stream-map (lambda (x) (list x (stream-car s))) (stream-cdr t))
  move diagonally right-down to (2 2) and recursively do the same thing
    (pairs (stream-cdr s) (stream-cdr t))

Would it be better to define pairs as the above and to filter for behavior like
i <= j? At the very least pairs should be renamed if its going to do any
filtering.

|#

#| Tests |#
(define-test (map (lambda (x) (stream-ref (all-pairs integers integers) x)) (iota 10))
             '((1 1) (1 2) (2 1) (1 3) (2 2) (1 4) (3 1) (1 5) (2 3) (1 6)))