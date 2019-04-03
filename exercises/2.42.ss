#|

Exercise 2.42: The "eight-queens puzzle" asks how to place
eight queens on a chessboard so that no queen is in check
from any other (i.e., no two queens are in the same row,
column, or diagonal). One possible solution is shown in
Figure 2.8. One way to solve the puzzle is to work across
the board, placing a queen in each column. Once we have
placed k - 1 queens, we must place the k^(th) queen in a
position where it does not check any of the queens already
on the board. We can formulate this approach recursively:
Assume that we have already generated the sequence of all
possible ways to place k - 1 queens in the first k - 1
columns of the board. For each of these ways, generate an
extended set of positions by placing a queen in each row of
the k^(th) column. Now filter these, keeping only the
positions for which the queen in the k^(th) column is safe
with respect to the other queens. This produces the sequence
of all ways to place k queens in the first k columns. By
continuing this process, we will produce not only one
solution, but all solutions to the puzzle.

Figure 2.8: A solution to the eight-queens puzzle.

+---+---+---+---+---+---+---+---+
|   |   |   |   |   | Q |   |   |
+---+---+---+---+---+---+---+---+
|   |   | Q |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
| Q |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   | Q |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   | Q |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   | Q |
+---+---+---+---+---+---+---+---+
|   | Q |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   | Q |   |   |   |   |
+---+---+---+---+---+---+---+---+

Figure 2.8: A solution to the eight-queens puzzle.

We implement this solution as a procedure "queens", which
returns a sequence of all solutions to the problem of
placing n queens on an n × n chessboard. "queens" has an
internal procedure "queen-cols" that returns the sequence of
all ways to place queens in the first k columns of the
board.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

In this procedure "rest-of-queens" is a way to place k - 1
queens in the first k - 1 columns, and "new-row" is a
proposed row in which to place the queen for the k^(th)
column. Complete the program by implementing the
representation for sets of board positions, including the
procedure "adjoin-position", which adjoins a new row-column
position to a set of positions, and "empty-board", which
represents an empty set of positions. You must also write
the procedure "safe?", which determines for a set of
positions, whether the queen in the k^(th) column is safe
with respect to the others. (Note that we need only check
whether the new queen is safe---the other queens are already
guaranteed safe with respect to each other.)

|#

#| Code from book |#
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

#| Answer |#
(load-ex "2.40")

(define empty-board '())

(define (adjoin-position row col rest-of-queens)
  (append (list (make-pos row col)) rest-of-queens))

(define (make-pos row col)
  (cons row col))

(define (pos-row pos)
  (car pos))

(define (pos-col pos)
  (cdr pos))

(define (none? p seq)
  (null? (filter p seq)))

(define (safe? k positions)
  (let* ([queen-in-kth (car (filter (lambda (p) (= (pos-col p) k)) positions))]
         [col (pos-col queen-in-kth)]
         [row (pos-row queen-in-kth)]
         [others (remove queen-in-kth positions)])
    (and (none? (lambda (other) (= (pos-row other) row)) others)
         (none? (lambda (other) (= (pos-col other) col)) others)
         (none? (lambda (other) (= (abs (- col (pos-col other))) (abs (- row (pos-row other))))) others))))

#| Tests |#
(define-test (queens 1) '(((1 . 1))))
(define-test (queens 2) '())
(define-test (queens 3) '())
(define-test (queens 4) '(((3 . 4) (1 . 3) (4 . 2) (2 . 1))
                          ((2 . 4) (4 . 3) (1 . 2) (3 . 1))))
(define-test (length (queens 5)) 10)
(define-test (length (queens 6)) 4)
(define-test (length (queens 7)) 40)
(define-test (length (queens 8)) 92)