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

(load-ex "5.45")

#| Answer 

  Interpreter (explicit-control evaluator) (5.27)
    * pushes: 56 * fib(n+1) - 34
    * max-depth: 5n + 3

  Compiled:
    * pushes: 8 * fib(n+1) - 6
    * max depth: 2n
  
  The interpreter vs. compiled is 56*fib(n+1) to 8*fib(n+1).

|#

#| Notes 

> (map factorial-tree-recur-compile-test (cdr (iota 10)))
(("fib(1)" 1 . "pushes=2, max-depth=2") 
 ("fib(2)" 1 . "pushes=10, max-depth=4")
 ("fib(3)" 2 . "pushes=18, max-depth=6")
 ("fib(4)" 3 . "pushes=34, max-depth=8")
 ("fib(5)" 5 . "pushes=58, max-depth=10")
 ("fib(6)" 8 . "pushes=98, max-depth=12")
 ("fib(7)" 13 . "pushes=162, max-depth=14")
 ("fib(8)" 21 . "pushes=266, max-depth=16")
 ("fib(9)" 34 . "pushes=434, max-depth=18"))

   (fib 1) => 2
   (fib 2) => 10
   (fib 3) => 18, (18-10-2) = 6
   (fib 4) => 34, (34-18-10) = 6

   S(n) = a * fib(n + 1) + b
   S(n-1) = a * fib(n) + b
   S(n-2) = a * fib(n-1) + b
   [a * fib(n + 1) + b] = [a * fib(n-1) + b] + [a * fib(n) + b] + 6
   [a * fib(n + 1) + b] = a * [fib(n-1) + fib(n)] + 2b + 6
   [a * fib(n + 1) + b] = a * fib(n+1) + 2b + 6
                      b = 2b + 6
                     -6 = b

   S(n) = a * fib(n + 1) - 6

   S(8) = 266 = a * 34 - 6
          8 = a

|#

(define (factorial-tree-recur-compile-test n)
  (define fact-exp
    `(begin 
      (define (fib n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))
      (fib ,n)))
  (cons (format "fib(~a)" n) (compile-test fact-exp)))