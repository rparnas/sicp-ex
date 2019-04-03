#|

Exercise 5.27: For comparison with Exercise 5.26, explore
the behavior of the following procedure for computing
factorials recursively:

(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))

By running this procedure with the monitored stack,
determine, as a function of n, the maximum depth of the
stack and the total number of pushes used in evaluating n!
for n >= 1. (Again, these functions will be linear.)
Summarize your experiments by filling in the following table
with the appropriate expressions in terms of n:

               Maximum depth       Number of pushes

Recursive
factorial

Iterative
factorial

The maximum depth is a measure of the amount of space used
by the evaluator in carrying out the computation, and the
number of pushes correlates well with the time required.

|#

(load-ex "5.26")

#| Answer

recursive factorial:
  * pushes: 32n - 10
  * max depth: 5n + 3

iterative factorial:
  * pushes: (n+1) * 35
  * max depth: 10

> (map factorial-recur-test (cdr (iota 10)))
(("1!" 1      . "pushes=22, max-depth=8") 
 ("2!" 2      . "pushes=54, max-depth=13")
 ("3!" 6      . "pushes=86, max-depth=18")
 ("4!" 24     . "pushes=118, max-depth=23")
 ("5!" 120    . "pushes=150, max-depth=28")
 ("6!" 720    . "pushes=182, max-depth=33")
 ("7!" 5040   . "pushes=214, max-depth=38")
 ("8!" 40320  . "pushes=246, max-depth=43")
 ("9!" 362880 . "pushes=278, max-depth=48"))

|#

(define (factorial-recur-test n)
  (define fact-exp
    `(begin 
      (define (factorial n)
        (if (= n 1)
            1 
            (* (factorial (- n 1)) n)))
      (factorial ,n)))
  (cons (format "~a!" n) (eval-stack-stats fact-exp)))