#|

Exercise 2.26: Suppose we define "x" and "y" to be two
lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

What result is printed by the interpreter in response to
evaluating each of the following expressions:

(append x y)
(cons x y)
(list x y)

|#

#| Tests |#
(define x (list 1 2 3))
(define y (list 4 5 6))

; append is "inside the trailing cdr of x, put y"
(define-test (append x y) '(1 2 3 4 5 6))

; cons is "create a new pair, car of x, cdr of y"
(define-test (cons x y) '((1 2 3) 4 5 6))

; list is "create a new list with the given elements"
(define-test (list x y) '((1 2 3) (4 5 6)))
