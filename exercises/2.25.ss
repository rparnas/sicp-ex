#|

Exercise 2.25: Give combinations of "car"s and "cdr"s that
will pick 7 from each of the following lists:

(1 3 (5 7) 9)
((7))
(1 (2 (3 (4 (5 (6 7))))))

|#

#| Tests |#
(define-test (car (cdaddr '(1 3 (5 7) 9))) 7)
(define-test (caar '((7))) 7)
(define-test (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))) 7)
