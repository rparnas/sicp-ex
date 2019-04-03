#|

Exercise 2.53: What would the interpreter print in response
to evaluating each of the following expressions?

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

|#

#| Tests |#
(define-test (list 'a 'b 'c) '(a b c))
(define-test (list (list 'george)) '((george)))
(define-test (cdr '((x1 x2) (y1 y2))) '((y1 y2)))
(define-test (cadr '((x1 x2) (y1 y2))) '(y1 y2))
(define-test (pair? (car '(a short list))) #f)
(define-test (memq 'red '((red shoes) (blue socks))) #f)
(define-test (memq 'red '(red shoes blue socks)) '(red shoes blue socks))
