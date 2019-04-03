#|

Exercise 2.31: Abstract your answer to Exercise 2.30 to
produce a procedure "tree-map" with the property that
"square-tree" could be defined as

(define (square-tree tree) (tree-map square tree))

|#

#| Answer |#
(define (map-tree f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree)
  (map-tree square tree))

#| Tests |#
(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define-test (square-tree t) '(1 (4 (9 16) 25) (36 49)))
