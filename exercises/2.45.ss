#|

Exercise 2.45: "right-split" and "up-split" can be expressed
as instances of a general splitting operation. Define a
procedure "split" with the property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))

produces procedures "right-split" and "up-split" with the
same behaviors as the ones already defined.

|#

#| Answer |#
(define (split a b)
  (define (iter paitner n)
    (if (= n 0)
        painter
        (let ([smaller (iter painter (- n 1))])
          (a painter (b smaller smaller)))))
  iter)