#|

Exercise 2.37:  Suppose we represent vectors v = (v_i) as
sequences of numbers, and matrices m = (m_(ij)) as sequences
of vectors (the rows of the matrix). For example, the matrix

+-         -+
|  1 2 3 4  |
|  4 5 6 6  |
|  6 7 8 9  |
+-         -+

is represented as the sequence "((1 2 3 4) (4 5 6 6) (6 7 8
9))". With this representation, we can use sequence
operations to concisely express the basic matrix and vector
operations. These operations (which are described in any
book on matrix algebra) are the following:

                                       __
(dot-product v w)      returns the sum >_i v_i w_i

(matrix-*-vector m v)  returns the vector t,
                                   __
                       where t_i = >_j m_(ij) v_j

(matrix-*-matrix m n)  returns the matrix p,
                                      __
                       where p_(ij) = >_k m_(ik) n_(kj)

(transpose m)          returns the matrix n,
                       where n_(ij) = m_(ji)

We can define the dot product as

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

Fill in the missing expressions in the following procedures
for computing the other matrix operations. (The procedure
"accumulate-n" is defined in Exercise 2.36.)

(define (matrix-*-vector m v)
  (map <??> m))
(define (transpose mat)
  (accumulate-n <??> <??> mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <??> m)))

|#

