#|

Exercise 2.65: Use the results of Exercise 2.63 and Exercise
2.64 to give Theta(n) implementations of "union-set" and
"intersection-set" for sets implemented as (balanced) binary
trees.

|#

#| Answer |#
(load-ex "2.62")
(load-ex "2.64")
(no-regression)

; union-set -- wrote Theta(n) version in 2.62
; intersection-set -- book told us Theta(n) version around 2.62.
; tree->list-2 -- analysed as Theta(n) in 2.63
; list->tree -- analysed as Theta(n) in 2.64

(define (union-set-tree set0 set1)
  (list->tree (union-set (tree->list-2 set0) 
                         (tree->list-2 set1))))

(define (intersection-set-tree set0 set1)
  (list->tree (intersection-set (tree->list-2 set0) 
                                (tree->list-2 set1))))

#| Tests |#
(define t0 (list->tree '(1 2 3 4)))
(define t1 (list->tree '(3 4 5 6)))
(define-test (union-set-tree t0 t1) 
             '(3 (1 () (2 () ())) (5 (4 () ()) (6 () ()))))
(define-test (intersection-set-tree t0 t1)
             '(3 () (4 () ())))
