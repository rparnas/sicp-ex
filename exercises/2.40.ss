#|

Exercise 2.40: Define a procedure "unique-pairs" that, given
an integer n, generates the sequence of pairs (i, j) with 1
<= j < i <= n. Use "unique-pairs" to simplify the definition
of "prime-sum-pairs" given above.

|#

#| Code from book |#
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

#| Answer |#
(load-ex "1.21")

(define (flatmap p seq)
  (fold-right append '() (map p seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) 
             	    (list i j))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map (lambda (pair) 
  	     (append pair (+ (car pair) (cadr pair))))
       (filter prime-sum? (unique-pairs n))))

#| Tests |#
(define-test (prime-sum-pairs 5) 
             '((2 1 . 3) (3 2 . 5) (4 1 . 5) (4 3 . 7) (5 2 . 7)))