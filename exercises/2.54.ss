#|

Exercise 2.54: Two lists are said to be "equal?" if they
contain equal elements arranged in the same order. For
example,

(equal? '(this is a list) '(this is a list))

is true, but

(equal? '(this is a list) '(this (is a) list))

is false. To be more precise, we can define "equal?"
recursively in terms of the basic "eq?" equality of symbols
by saying that "a" and "b" are "equal?" if they are both
symbols and the symbols are "eq?", or if they are both lists
such that "(car a)" is "equal?" to "(car b)" and "(cdr a)"
is "equal?" to "(cdr b)". Using this idea, implement
"equal?" as a procedure.

|#

#| Answer |#
(define (equal? a b)
  (cond [(or (null? a) (null? b)) 
         (and (null? a) (null? b))]
        [(and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))]
        [(and (not (pair? a)) (not (pair? b)))
         (= a b)]
        [else 
         #f]))

#| Tests |#
(define a0 '(1 2 3))
(define a1 '(1 2 3))
(define b0 '(4 5 6))
(define-test (equal? '() '()) #t)
(define-test (equal? '() '(5)) #f)
(define-test (equal? '(5) '()) #f)
(define-test (equal? a0 a1) #t)
(define-test (equal? a0 b0) #f)
