#|

Exercise 1.12: The following pattern of numbers is called
Pascal's triangle.

        1
      1   1
    1   2   1
  1   3   3   1
1   4   6   4   1
      . . .

The numbers at the edge of the triangle are all 1, and each
number inside the triangle is the sum of the two numbers
above it. Write a procedure that computes elements of
Pascal's triangle by means of a recursive process.

|#

(define (a-to-b start stop)
  (if (> start stop) 
      '()
      (cons start (a-to-b (+ start 1) stop))))

(define (pascal-cell row col)
  (if (or (= col 0) (= col row))
      1
      (+ (pascal-cell (- row 1) (- col 1)) (pascal-cell (- row 1) col))))
      
(define (pascal-row row)
  (map (lambda (col) (pascal-cell row col)) (a-to-b 0 row)))
  
(define (pascal rows)
  (if (< rows 0) 
      '()
      (append (pascal (- rows 1)) (list (pascal-row rows)))))
        
#| Tests |#
(define-test (pascal 6)
             '((1)
               (1 1)
               (1 2 1)
               (1 3 3 1)
               (1 4 6 4 1)
               (1 5 10 10 5 1)
               (1 6 15 20 15 6 1)))
