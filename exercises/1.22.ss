#|

Exercise 1.22: Most Lisp implementations include a primitive
called "runtime" that returns an integer that specifies the
amount of time the system has been running (measured, for
example, in microseconds). The following "timed-prime-test"
procedure, when called with an integer n, prints n and
checks to see if n is prime. If n is prime, the procedure
prints three asterisks followed by the amount of time used
in performing the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

Using this procedure, write a procedure "search-for-primes"
that checks the primality of consecutive odd integers in a
specified range. Use your procedure to find the three
smallest primes larger than 1000; larger than 10,000; larger
than 100,000; larger than 1,000,000. Note the time needed to
test each prime. Since the testing algorithm has order of
growth of Theta(sqrt(n)), you should expect that testing for
primes around 10,000 should take about sqrt(10) times as
long as testing for primes around 1000. Do your timing data
bear this out? How well do the data for 100,000 and
1,000,000 support the Theta(sqrt(n)) prediction? Is your
result compatible with the notion that programs on your
machine run in time proportional to the number of steps
required for the computation?

|#

#| Answer |#

(load-ex "1.21")

(define (search-for-primes start)
  (define (iter n)
    (if (prime? n) n (iter (+ n 2))))
    (iter (if (odd? start) start (+ 1 start))))

#| Tests |#

(define (percent-error approx exact)
    (if (= exact 0) 
        0
        (* (/ (- approx exact) exact) 100)))

(define (finder-test start levels count finder expected-growth)
  (define expected '())
  (define starts (map (lambda (level) (* start (expt 10 level))) (iota  levels)))
  ; return count prime results in the format (<report> <report>...)
  (define (iter start result)
    (if (>= (length result) count)
        result
        (let ([r (get-report (lambda () (finder start)) expected)])
          (iter (+ 2 (car r)) (append result (list r))))))
  ; body
  (for-each (lambda (start)
              (let* ([reports (iter start '())]
                     [avg-elapsed (/ (fold-right (lambda (a b) (+ (cadr a) b)) (+) reports) (length reports))])
                (display "\r\n")
                (display (format "From ~a\r\n" start))
                (for-each (lambda (r) (display-report r)) reports)
                (set! expected (* avg-elapsed (expected-growth 10)))))
            starts))

(define (sum ls)
  (if (null? ls) 0 (+ (car ls) (sum (cdr ls)))))

(define (avg ls)
  (/ (sum ls) (length ls)))

(define (display-report report)
  (define prime (car report))
  (define actual (cadr report))
  (define expected (caddr report))
  (if (null? expected)
      (display (format "~a | Actual: ~ams\r\n" prime (round actual)))
      (display (format "~a | Expected: ~ams | Actual: ~ams | Error: ~a%\r\n" 
                  prime 
                  (round expected) 
                  (round actual) 
                  (round (percent-error expected actual))))))

(define (get-report thunk expected)
  (define start (cpu-time))
  (define result (thunk))
  (define stop (cpu-time))
  (define elapsed (- stop start))
  (list result elapsed expected))

#| Tests -- manual

> (finder-test (expt 10 15) 3 3 search-for-primes sqrt)
From 1000000000000000
1000000000000037 | Actual: 578ms
1000000000000091 | Actual: 422ms
1000000000000159 | Actual: 500ms

From 10000000000000000
10000000000000061 | Expected: 1581.0ms | Actual: 2078ms | Error: -24.0%
10000000000000069 | Expected: 1581.0ms | Actual: 1265ms | Error: 25.0%
10000000000000079 | Expected: 1581.0ms | Actual: 1266ms | Error: 25.0%

From 100000000000000000
100000000000000003 | Expected: 4858.0ms | Actual: 4172ms | Error: 16.0%
100000000000000013 | Expected: 4858.0ms | Actual: 4172ms | Error: 16.0%
100000000000000019 | Expected: 4858.0ms | Actual: 4109ms | Error: 18.0%

|#
