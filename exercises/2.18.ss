#|

Exercise 2.18: Define a procedure "reverse" that takes a
list as argument and returns a list of the same elements in
reverse order:

(reverse (list 1 4 9 16 25))
 (25 16 9 4 1) 

|#

#| Answser |#
(define (reverse ls)
  (define (iter ls result)
    (if (null? ls)
        result
        (iter (cdr ls) (append (list (car ls)) result))))
  (iter ls '()))

#| Tests |#
(define-test (reverse (list 1 4 9 16 25)) '(25 16 9 4 1))
