#|

Exercise 3.57: How many additions are performed when we
compute the n^(th) Fibonacci number using the definition of
"fibs" based on the "add-streams" procedure? Show that the
number of additions would be exponentially greater if we had
implemented "(delay <exp>)" simply as "(lambda () <exp>)",
without using the optimization provided by the "memo-proc"
procedure described in Section 3.5.1.

|#

(load-ex "3.56")

#| Code from book |#
(define fibs
  (cons-stream 0
    (cons-stream 1
      (add-streams (stream-cdr fibs)
                   fibs))))

#| Answer 

Each additional index involes one additional add. (n-2) adds will have happend
for n^(th) Fibonacci number as the first two numbers are pre-defined.

If memoization is not included, each new index recursively goes through the
stream from the begining. The upper bound of additions is Theta(2^n) the
exact value being Theta(golden-ratio^n) or about Theta(1.6^n).

The total additions involved to get to any index n >= 2 is exactly fib(n) - 1.

|#

