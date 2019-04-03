#|

Exercise 2.22: Louis Reasoner tries to rewrite the first
"square-list" procedure of Exercise 2.21 so that it evolves
an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

Unfortunately, defining "square-list" this way produces the
answer list in the reverse order of the one desired. Why?

Louis then tries to fix his bug by interchanging the
arguments to "cons":

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

This doesn't work either. Explain.

|#

#| Answer 

1st square-list
(square-list '(1 2 3))
(iter '(1 2 3) '())
(iter '(2 3) '(1))
(iter '(3) '(4 1))
(iter '() '(9 4 1))

Each iteration you put a new answer in the car of a new pair
and all previous answers in the cdr of the new pair. You are
pushing each elment onto the begining of the list as you go
reversing the list.

2nd square-list
(square-list-2 '(1 2 3))
(iter '(1 2 3) '())
(iter '(2 3) '(() . 1))
(iter '(3) '((() . 1) . 4))
(iter '() '(((() . 1) . 4) . 9))

Each iterations you put the previous accumulated answers in
the car of a new pair and the new answer in the cdr of a new
pair.

|#
