#|

Exercise 4.41: Write an ordinary Scheme program to solve the
multiple dwelling puzzle.

|#

(load-ex "2.41") ; permutations

#| Answer |#
(define-test (filter (lambda (floors)
                       (let ([baker (car floors)]
                             [cooper (cadr floors)]
                             [fletcher (caddr floors)]
                             [miller (cadddr floors)]
                             [smith (car (cddddr floors))])
                        (and 
                         (not (= baker 5))
                         (not (= cooper 1))
                         (not (= fletcher 5))
                         (not (= fletcher 1))
                         (> miller cooper)
                         (not (= (abs (- smith fletcher)) 1))
                         (not (= (abs (- fletcher cooper)) 1)))))
            (permutations '(1 2 3 4 5)))
            '((3 2 4 5 1)))