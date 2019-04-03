#|

Exercise 1.24: Modify the "timed-prime-test" procedure of
Exercise 1.22 to use "fast-prime?" (the Fermat method), and
test each of the 12 primes you found in that exercise. Since
the Fermat test has Theta(log n) growth, how would you
expect the time to test primes near 1,000,000 to compare
with the time needed to test primes near 1000? Do your data
bear this out? Can you explain any discrepancy you find?

|#

#| Code from 1.2.6 |#
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n) 
   (define (try-it a) 
     (= (expmod a n n) a)) 
   (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
   (cond ((= times 0) #t)
         ((fermat-test n) (fast-prime? n (- times 1)))
         (else #f)))

#| Answer |#

(load-ex "1.23")
(define (prime? n)
  (fast-prime? n 5))

#| 

It is much faster although it is now using a probabalistic
prime test.

|#

#| Tests -- manual

> (finder-test (expt 10 15) 3 3 search-for-primes sqrt)
From 1000000000000000
1000000000000037 | Actual: 0ms
1000000000000091 | Actual: 0ms
1000000000000159 | Actual: 0ms

From 10000000000000000
10000000000000061 | Expected: 0ms | Actual: 0ms | Error: 0%
10000000000000069 | Expected: 0ms | Actual: 0ms | Error: 0%
10000000000000079 | Expected: 0ms | Actual: 0ms | Error: 0%

From 100000000000000000
100000000000000003 | Expected: 0ms | Actual: 0ms | Error: 0%
100000000000000013 | Expected: 0ms | Actual: 0ms | Error: 0%
100000000000000019 | Expected: 0ms | Actual: 0ms | Error: 0%

|#
