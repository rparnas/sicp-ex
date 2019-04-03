#|

Exercise 2.28: Write a procedure "fringe" that takes as
argument a tree (represented as a list) and returns a list
whose elements are all the leaves of the tree arranged in
left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))
(fringe x)
 (1 2 3 4) 
(fringe (list x x))
 (1 2 3 4 1 2 3 4) 

|#

#| Answer |#
(define (fringe ls)
  (define (leaf? x)
    (not (pair? x)))
  (define (iter ls result)
    (if (null? ls)
        result
        (iter (cdr ls)
              (append result ((if (leaf? (car ls)) list fringe) (car ls))))))
  (iter ls '()))

#| Tests |#
(define x (list (list 1 2) (list 3 4)))
(define-test (fringe (list x x)) '(1 2 3 4 1 2 3 4))
