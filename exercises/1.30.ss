#|

Exercise 1.30: The "sum" procedure above generates a linear
recursion. The procedure can be rewritten so that the sum is
performed iteratively. Show how to do this by filling in the
missing expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if <??>
        <??>
        (iter <??> <??>)))
  (iter <??> <??>))

|#

#| Answer |#
(load-ex "1.29")

(define (sum term a next b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n) (+ result (term n)))))
  (iter a 0))