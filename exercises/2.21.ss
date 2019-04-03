#|

Exercise 2.21: The procedure "square-list" takes a list of
numbers as argument and returns a list of the squares of
those numbers.

(square-list (list 1 2 3 4))
 (1 4 9 16) 

Here are two different definitions of "square-list".
Complete both of them by filling in the missing expressions:

(define (square-list items)
  (if (null? items)
      nil
      (cons <??> <??>)))
(define (square-list items)
  (map <??> <??>))

|#

(define (square-list-a items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-a (cdr items)))))

(define (square-list-b items)
  (map square items))

#| Tests |#
(define-test (square-list-a (list 1 2 3 4)) '(1 4 9 16))
(define-test (square-list-b (list 1 2 3 4)) '(1 4 9 16))
