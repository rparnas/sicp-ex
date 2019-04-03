#|

Exercise 2.30: Define a procedure "square-tree" analogous to
the "square-list" procedure of Exercise 2.21. That is,
"square-tree" should behave as follows:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
 (1 (4 (9 16) 25) (36 49)) 

Define "square-tree" both directly (i.e., without using any
higher-order procedures) and also by using "map" and
recursion.

|#

#| Answer |#
(define (square-tree0 tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (square tree)]
        [else (cons (square-tree0 (car tree))
                    (square-tree0 (cdr tree)))]))

(define (square-tree1 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree1 sub-tree)
             (square sub-tree)))
       tree))

#| Tests |#
(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define-test (square-tree0 t) '(1 (4 (9 16) 25) (36 49)))
(define-test (square-tree1 t) '(1 (4 (9 16) 25) (36 49)))
