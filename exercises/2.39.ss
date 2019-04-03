#|

Exercise 2.39: Complete the following definitions of
"reverse" (Exercise 2.18) in terms of "fold-right" and
"fold-left" from Exercise 2.38:

(define (reverse sequence)
  (fold-right (lambda (x y) <??>) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) <??>) nil sequence))

|#

(define (reverse0 s)
  (fold-right (lambda (cur result) (append result (list cur))) '() s))

(define (reverse1 s)
  (fold-left (lambda (result cur) (cons cur result)) '() s))

#| Tests |#
(define ls '(1 2 3 4 5))
(define-test (reverse ls) '(5 4 3 2 1))
(define-test (reverse0 ls) '(5 4 3 2 1))
(define-test (reverse1 ls) '(5 4 3 2 1))
