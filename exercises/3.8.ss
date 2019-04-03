#|

Exercise 3.8: When we defined the evaluation model in
Section 1.1.3, we said that the first step in evaluating an
expression is to evaluate its subexpressions. But we never
specified the order in which the subexpressions should be
evaluated (e.g., left to right or right to left). When we
introduce assignment, the order in which the arguments to a
procedure are evaluated can make a difference to the result.
Define a simple procedure "f" such that evaluating

(+ (f 0) (f 1))

will return 0 if the arguments to "+" are evaluated from
left to right but will return 1 if the arguments are
evaluated from right to left.

|#

; f(0) + f(1) --> 0
; f(1) + f(0) --> 1

; the 0th, 2nd, 4th, etc. call returns 0.
; the 1th, 2nd, 4rd, etc. call returns the argument previously invoked.
(define f
  (let ([call-count 0]
        [last-x #f])
    (lambda (x)
      (let ([result (if (even? call-count) 0 last-x)])
        (set! call-count (+ 1 call-count))
        (set! last-x x)
        result))))

#| Tests

> (f 0)
0
> (f 1)
0

> (f 1)
0
> (f 0)
1

|#