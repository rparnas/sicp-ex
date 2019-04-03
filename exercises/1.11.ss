#|

Exercise 1.11: A function f is defined by the rule that

       /
       | n                           if n < 3
f(n) = < f(n-1) + 2f(n-2) + 3f(n-3)  if n >= 3
       |
       \

Write a procedure that computes f by means of a recursive
process. Write a procedure that computes f by means of an
iterative process.

|#

#| Answer |#

(define (recursive n)
  (define f recursive)
  (if (< n 3) 
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (iterative n)
  (define (f 1ago 2ago 3ago n)
    (define now (+ 1ago (* 2 2ago) (* 3 3ago)))
    (if (= n 0)
      now
      (f now 1ago 2ago (- n 1))))
  (if (< n 3) n (f 2 1 0 (- n 3))))

(define-test (recursive 7) 142)
(define-test (iterative 7) 142)
