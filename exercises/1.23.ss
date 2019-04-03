#|

Exercise 1.23: The "smallest-divisor" procedure shown at the
start of this section does lots of needless testing: After
it checks to see if the number is divisible by 2 there is no
point in checking to see if it is divisible by any larger
even numbers. This suggests that the values used for
"test-divisor" should not be 2, 3, 4, 5, 6,..., but rather
2, 3, 5, 7, 9,.... To implement this change, define a
procedure "next" that returns 3 if its input is equal to 2
and otherwise returns its input plus 2. Modify the
"smallest-divisor" procedure to use "(next test-divisor)"
instead of "(+ test-divisor 1)". With "timed-prime-test"
incorporating this modified version of "smallest-divisor",
run the test for each of the 12 primes found in Exercise
1.22. Since this modification halves the number of test
steps, you should expect it to run about twice as fast. Is
this expectation confirmed? If not, what is the observed
ratio of the speeds of the two algorithms, and how do you
explain the fact that it is different from 2?

|#

#| Answer |#
(load-ex "1.22")

(define (smallest-divisor n)
  (cond [(= n 2) 2]
        [(even? n) (find-divisor n 2 (lambda (test-divisor) (+ test-divisor 1)))]
        [else      (find-divisor n 3 (lambda (test-divisor) (+ test-divisor 2)))]))

(define (find-divisor n test-divisor get-next-test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (get-next-test-divisor test-divisor) get-next-test-divisor)]))

#|

It is twice as fast. If your implementation checks which
next function is used every single iteration, it may not be
so.

|#

#| Tests -- manual

> (finder-test (expt 10 15) 3 3 search-for-primes sqrt)
From 1000000000000000
1000000000000037 | Actual: 313ms
1000000000000091 | Actual: 218ms
1000000000000159 | Actual: 266ms

From 10000000000000000
10000000000000061 | Expected: 840.0ms | Actual: 1109ms | Error: -24.0%
10000000000000069 | Expected: 840.0ms | Actual: 688ms | Error: 22.0%
10000000000000079 | Expected: 840.0ms | Actual: 687ms | Error: 22.0%

From 100000000000000000
100000000000000003 | Expected: 2618.0ms | Actual: 2188ms | Error: 20.0%
100000000000000013 | Expected: 2618.0ms | Actual: 2187ms | Error: 20.0%
100000000000000019 | Expected: 2618.0ms | Actual: 2188ms | Error: 20.0%

|#