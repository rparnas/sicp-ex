#|

Exercise 5.46: Carry out an analysis like the one in
Exercise 5.45 to determine the effectiveness of compiling
the tree-recursive Fibonacci procedure

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

compared to the effectiveness of using the special-purpose
Fibonacci machine of Figure 5.12. (For measurement of the
interpreted performance, see Exercise 5.29.) For Fibonacci,
the time resource used is not linear in n; hence the ratios
of stack operations will not approach a limiting value that
is independent of n.

|#

