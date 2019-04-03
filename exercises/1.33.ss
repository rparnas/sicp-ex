#|

Exercise 1.33: You can obtain an even more general version
of "accumulate" (Exercise 1.32) by introducing the notion of
a filter on the terms to be combined. That is, combine only
those terms derived from values in the range that satisfy a
specified condition. The resulting "filtered-accumulate"
abstraction takes the same arguments as accumulate, together
with an additional predicate of one argument that specifies
the filter. Write "filtered-accumulate" as a procedure. Show
how to express the following using "filtered-accumulate":

a. the sum of the squares of the prime numbers in the
interval a to b (assuming that you have a "prime?" predicate
already written)

b. the product of all the positive integers less than n that
are relatively prime to n (i.e., all positive integers i < n
such that GCD(i, n) = 1).

|#

#| Answer |#

(load-ex "1.21")

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond [(> a b) result]
          [(filter a) (iter (next a) (combiner result (term a)))]
          [else (iter (next a) result)]))
  (iter a null-value))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a (lambda (x) (+ 1 x)) b prime?))

(define (relative-prime? i n)
  (= (gcd i n) 1))

(define (product-of-relative-primes n)
  (filtered-accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ 1 x)) n (lambda (i) (relative-prime? i n))))

#| Tests |#
(define-test (sum-of-prime-squares 1 5) 38)
(define-test (product-of-relative-primes 10) 189)
