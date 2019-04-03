#|

Exercise 2.41: Write a procedure to find all ordered triples
of distinct positive integers i, j, and k less than or equal
to a given integer n that sum to a given integer s.

|#

#| Answer (*) |#
(load-ex "2.40")

(define (sum ls)
  (fold-left + (+) ls))

(define (permutations set)
  (if (null? set)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (p) 
                      (cons x p))
                      (permutations (remove x set))))
               set)))

(define (unique-nples n count)
  (if (= count 0)
      '(())
      (flatmap (lambda (i)
                 (map (lambda (j) (cons i j))
                      (unique-nples (- i 1) (- count 1))))
               (enumerate-interval 1 n))))

(define (unique-triples-that-sum-to-s n s)
  (filter (lambda (tri) (= (sum tri) s))
   (unique-nples n 3)))

#| Tests |#
(define-test (unique-triples-that-sum-to-s 10 10) 
            '((5 3 2) (5 4 1) (6 3 1) (7 2 1)))
