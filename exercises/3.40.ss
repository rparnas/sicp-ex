#|

Exercise 3.40: Give all possible values of "x" that can
result from executing

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

Which of these possibilities remain if we instead use
serialized procedures:

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

|#

#| Answer 

a: (set! x (* x x))
   a=x     ; action 0
   b=x     ; action 1
   x=(a*b) ; action 2

b: (set! x (* x x x))
  a=x ; action 3
  b=x ; action 4
  c=x ; action 5
  x=(* a b c) ; action 6

The possiblities are (100 1000 10000 100000 1000000)

When the two processes are serialzed the only possibility is 1000000.

if a runs first
  x=10
  x= x*x =100
  x= x*x*x = 1000000

if b runs first
  x=10
  x= x*x*x = 1000
  x = x*x = 1000000

This makes sense because multiplication is commutative.

|#

#| Tests |#
(load-ex "3.39")

(define (make-run_340)
  (let ([x 10] [a_a #f] [a_b #f] [b_a #f] [b_b #f] [b_c #f])
    (lambda (step)
      (cond [(equal? step 'return) x]
            [(= step 0) (set! a_a x)]
            [(= step 1) (set! a_b x)]
            [(= step 2) (set! x (* a_a a_b))]
            [(= step 3) (set! b_a x)]
            [(= step 4) (set! b_b x)]
            [(= step 5) (set! b_c x)]
            [(= step 6) (set! x (* b_a b_b b_c))]))))

(define-test (possible-results (possibilities make-run_340 7 '((0 1) (1 2) (3 4) (4 5) (5 6))))
             '(100 1000 10000 100000 1000000))
