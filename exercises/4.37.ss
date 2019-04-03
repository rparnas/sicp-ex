#|

Exercise 4.37: Ben Bitdiddle claims that the following
method for generating Pythagorean triples is more efficient
than the one in Exercise 4.35. Is he correct? (Hint:
Consider the number of possibilities that must be explored.)

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

|#

(load-ex "4.35")

#| Answer 

Algorithm 4.35 and 4.36 tries 220 possible (i k j) for 1...10.

Algorithm 4.37 explores 55 possible (i k j) for 1..10.

The new algorithm is more efficient in terms of how many sets of ambiguous value
possibilities are explored. A deeper analysis would take into account how
expensive other operations are (for example, is a sqrt costly and only some
possibilities sets bother to compute sqrt) but for this simple example we can
assume the ambiguity retry system predominates.

Details:

The sum of natural numbers 1..n is n(n+1)/2.

For 4.35 and 4.36, consider (i k j) i=1. For (1 1 k), k has 10 possibilities,
for (1 2 k) k has 9 possibilities and so on. For i=2, it will be the similar
except for (2 2 k) there k only has 9 possibilities. The total is (sum of 1..10)
+ (sum of 1..9) +... (sum of 1..1) possibilities or (55 + 45 + 36 + 28 + 21 + 15
+ 10 + 6 + 3 + 1) = 220 possibilities.

4.37 differs because the only ambiguous values are i and j. k is calculated from
those two values. For i=1, j=1..10. For i=2, j=2..10. The total possibilities
are just the (sum of 1..10) = 55 possibilities.

|#

(define setup-environment-436 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-436)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))
    ;;; code from book
    (add '(define (a-pythagorean-triple-between low high)
        (let ((i (an-integer-between low high))
              (hsq (* high high)))
          (let ((j (an-integer-between i high)))
            (let ((ksq (+ (* i i) (* j j))))
              (require (>= hsq ksq))
              (let ((k (sqrt ksq)))
                (require (integer? k))
                (list i j k)))))))
    ;;; tests
    (add '(define (a-pythagorean-triple-between-435 low high)
            (let ((i (an-integer-between low high)))
              (let ((j (an-integer-between i high)))
                (let ((k (an-integer-between j high)))
                  ; (require (= (+ (* i i) (* j j)) (* k k)))
                  (list i j k))))))
    (add '(define (all-pythagorean-triples-436)
        (let* ([k (an-integer-between 1 10)] ;;; modified b/c I know the final triple is (6 8 10)
               [j (an-integer-between 1 k)]
               [i (an-integer-between 1 j)])
          ; (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k))))
    (add '(define (a-pythagorean-triple-between-437 low high)
            (let ((i (an-integer-between low high))
                  (hsq (* high high)))
              (let ((j (an-integer-between i high)))
                (let ((ksq (+ (* i i) (* j j))))
                  ; (require (>= hsq ksq))
                  (let ((k (sqrt ksq)))
                    ; (require (integer? k))
                    (list i j k)))))))
    env))

#| Tests

> (length (eval-all '(a-pythagorean-triple-between-435 1 10))) ; 4.35
220
> (length (eval-all '(all-pythagorean-triples-436))) ; 4.36
220
> (length (eval-all '(a-pythagorean-triple-between-437 1 10))) ; 4.37
55

|#