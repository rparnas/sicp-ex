#|

Exercise 3.69: Write a procedure "triples" that takes three
infinite streams, S, T, and U, and produces the stream of
triples (S_i, T_j, U_k) such that i <= j <= k. Use "triples"
to generate the stream of all Pythagorean triples of
positive integers, i.e., the triples (i, j, k) such that i
<= j and i^2 + j^2 = k^2.

|#

(load-ex "3.68")

#| Answer |#
(define (triples s t u)
  (let ([floor (stream-map (lambda (p) (list (stream-car s) (car p) (cadr p)))
                           (pairs t u))])
    (cons-stream
      (stream-car floor)
      (interleave (stream-cdr floor)
                  (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define pythagorean-triples
  (stream-filter (lambda (p)
                   (let ([i (car p)] [j (cadr p)] [k (caddr p)])
                     (= (+ (* i i) (* j j)) (* k k))))
                 (triples integers integers integers)))

#| Notes 

~1st floor~
(1 1 1) (1 1 2) (1 1 3) (1 1 4)...
(  x  ) (1 2 2) (1 2 3) (1 2 4)...
(  x  ) (  x  ) (1 3 3) (1 3 4)...
(  x  ) (  x  ) (  x  ) (1 4 4)...

~2nd floor~
(  x  ) (  x  ) (  x  ) (  x  )...
(  x  ) (2 2 2) (2 2 3) (2 2 4)...
(  x  ) (  x  ) (3 3 3) (3 3 4)...
(  x  ) (  x  ) (  x  ) (4 4 4)

Getting pythagorean-triples takes a long time.

|#

#| Tests |#
(define-test (let ([t (triples integers integers integers)])
               (andmap (lambda (x)
                         (let* ([ref (stream-ref t x)]
                                [i (car ref)]
                                [j (cadr ref)]
                                [k (caddr ref)])
                         (<= i j k)))
                        (iota 1000)))
              #t)

(define-test (let ([t pythagorean-triples])
               (andmap (lambda (x)
                         (let* ([ref (stream-ref t x)]
                                [i (car ref)]
                                [j (cadr ref)]
                                [k (caddr ref)])
                         (= (+ (* i i) (* j j)) (* k k))))
                        (iota 4)))
              #t)
