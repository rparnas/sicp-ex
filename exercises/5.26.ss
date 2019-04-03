#|

Exercise 5.26: Use the monitored stack to explore the
tail-recursive property of the evaluator (Section 5.4.2).
Start the evaluator and define the iterative "factorial"
procedure from Section 1.2.1:

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

Run the procedure with some small values of n. Record the
maximum stack depth and the number of pushes required to
compute n! for each of these values.

a. You will find that the maximum depth required to evaluate
n! is independent of n. What is that depth?

b. Determine from your data a formula in terms of n for the
total number of push operations used in evaluating n! for
any n >= 1. Note that the number of operations used is a
linear function of n and is thus determined by two
constants.

|#

(load-ex "5.23") ; skip the normal-order evaluator machine from 5.25

#| Answer

a. Stack max-depth is 10.

b. Stack pushes is (n+1) * 35

NOTE: Other answers online show stack pushes as 35*n + 29, however I am probably
somehow using a slightly different evaluator.

> (map factorial-iter-test (cdr (iota 10)))
(("1!" 1      . "pushes=70, max-depth=10")
 ("2!" 2      . "pushes=105, max-depth=10")
 ("3!" 6      . "pushes=140, max-depth=10")
 ("4!" 24     . "pushes=175, max-depth=10")
 ("5!" 120    . "pushes=210, max-depth=10")
 ("6!" 720    . "pushes=245, max-depth=10")
 ("7!" 5040   . "pushes=280, max-depth=10")
 ("8!" 40320  . "pushes=315, max-depth=10")
 ("9!" 362880 . "pushes=350, max-depth=10"))

|#

(define (factorial-iter-test n)
  (define fact-exp
    `(begin 
      (define (factorial n)
        (define (iter product counter)
          (if (> counter n)
              product
              (iter (* counter product) (+ counter 1))))
        (iter 1 1))
      (factorial ,n)))
  (cons (format "~a!" n) (eval-stack-stats fact-exp)))

(define (eval-stack-stats exp)
  (let* ([m (eval-machine)])
    (set-register-contents! m 'exp exp)
    (set-register-contents! m 'env (setup-environment))
    (start m)
    (cons (get-register-contents m 'val)
          ((m 'stack) 'get-statistics))))