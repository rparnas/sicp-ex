#|

Exercise 3.39: Which of the five possibilities in the
parallel execution shown above remain if we instead
serialize execution as follows:

(define x 10)
(define s (make-serializer))
(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x))))))
 (s (lambda () (set! x (+ x 1)))))

|#

#| Answer 

a: (set! x ((s (lambda () (* x x))))))
   ===================================
   (serialized    ; action 0
     a= get x
     b= get x)
   x=(a*b)        ; action 1

b: (serialized    ; action 2
     a=x
     x=a+1)

The remaining possibilities are (100 101 121)

|#
(load-ex "3.38")

(define (make-run_339)
  (let ([x 10] [a_a #f] [a_b #f] [b_a #f])
    (lambda (step)
            (cond [(equal? step 'return) x]
                  [(= step 0) (set! a_a x)
                              (set! a_b x)]
                  [(= step 1) (set! x (* a_a a_b))]
                  [(= step 2) (set! b_a x)
                              (set! x (+ b_a 1))]))))

(define-test (possible-results (possibilities make-run_339 3 '((0 1))))
             '(100 101 121))
