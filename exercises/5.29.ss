#|

Exercise 5.29: Monitor the stack operations in the
tree-recursive Fibonacci computation:

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

a. Give a formula in terms of n for the maximum depth of the
stack required to compute (Fib)(n) for n >= 2. Hint: In
Section 1.2.2 we argued that the space used by this process
grows linearly with n.

b. Give a formula for the total number of pushes used to
compute (Fib)(n) for n >= 2. You should find that the number
of pushes (which correlates well with the time used) grows
exponentially with n. Hint: Let S(n) be the number of pushes
used in computing (Fib)(n). You should be able to argue that
there is a formula that expresses S(n) in terms of S(n - 1),
S(n - 2), and some fixed "overhead" constant k that is
independent of n. Give the formula, and say what k is. Then
show that S(n) can be expressed as a · Fib(n + 1) + b and
give the values of a and b.

|#

(load-ex "5.27") ; skip 5.28 b/c it disables tail recursion

#| Answer 

a. max-depth(n) = 5n + 3

b. pushes(n) = 56 * fib(n+1) - 34

   "begin" and "define" expressions => 4 pushes
   (fib 0) => 22
   (fib 1) => 22
   (fib 2) => 78, (78-22-22) = 34
   (fib 3) => 134, (134-78-22) = 34
   (fib 4) => 246, (246-134-78) = 34

   S(n) = S(n-2) + S(n-1) + 34

   S(n) = a * fib(n + 1) + b
   S(n-1) = a * fib(n) + b
   S(n-2) = a * fib(n-1) + b
   [a * fib(n + 1) + b] = [a * fib(n-1) + b] + [a * fib(n) + b] + 34
   [a * fib(n + 1) + b] = a * [fib(n-1) + fib(n)] + 2b + 34
   [a * fib(n + 1) + b] = a * fib(n+1) + 2b + 34
                      b = 2b + 34
                     -34 = b

   S(n) = a * fib(n + 1) - 34

   S(8) = 1870 = a * 34 - 34
          56 = a

Raw Data:
  ("fib(0)" 0 . "pushes=22, max-depth=8")
  ("fib(1)" 1 . "pushes=22, max-depth=8")
  ("fib(2)" 1 . "pushes=78, max-depth=13")
  ("fib(3)" 2 . "pushes=134, max-depth=18")
  ("fib(4)" 3 . "pushes=246, max-depth=23")
  ("fib(5)" 5 . "pushes=414, max-depth=28")
  ("fib(6)" 8 . "pushes=694, max-depth=33")
  ("fib(7)" 13 . "pushes=1142, max-depth=38")
  ("fib(8)" 21 . "pushes=1870, max-depth=43")
  ("fib(9)" 34 . "pushes=3046, max-depth=48")

|#

(define (factorial-tree-recur-test n)
  (define fact-exp
    `(begin 
      (define (fib n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))
      (fib ,n)))
  (cons (format "fib(~a)" n) (eval-stack-stats fact-exp)))