#|

Exercise 3.15: Draw box-and-pointer diagrams to explain the
effect of "set-to-wow!" on the structures "z1" and "z2"
above.

|#

#| Code from book |#
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

#| Answer -- on paper |#
