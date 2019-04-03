#|

Exercise 3.9: In Section 1.2.1 we used the substitution
model to analyze two procedures for computing factorials, a
recursive version

(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

and an iterative version

(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

Show the environment structures created by evaluating
"(factorial 6)" using each version of the "factorial"
procedure.

|#

#| Answer

Recursive:
(factorial 6) -- Frame[n: 6]
(factorial 5) -- Frame[n: 5]
(factorial 4) -- Frame[n: 4]
(factorial 3) -- Frame[n: 3]
(factorial 2) -- Frame[n: 2]
(factorial 1) -- Frame[n: 1]

Iterative:
(factorial 6) -- Frame[n: 6]
(fact-iter   1 1 6) -- Frame[product:   1, counter: 1, max-count: 6]
(fact-iter   1 2 6) -- Frame[product:   1, counter: 2, max-count: 6]
(fact-iter   2 3 6) -- Frame[product:   2, counter: 3, max-count: 6]
(fact-iter   6 4 6) -- Frame[product:   6, counter: 4, max-count: 6]
(fact-iter  24 5 6) -- Frame[product:  24, counter: 5, max-count: 6]
(fact-iter 120 6 6) -- Frame[product: 120, counter: 6, max-count: 6]
(fact-iter 720 7 6) -- Frame[product: 720, counter: 7, max-count: 6]

|#
