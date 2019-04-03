#|

Exercise 3.62: Use the results of Exercise 3.60 and Exercise
3.61 to define a procedure "div-series" that divides two
power series. "div-series" should work for any two series,
provided that the denominator series begins with a nonzero
constant term. (If the denominator has a zero constant term,
then "div-series" should signal an error.) Show how to use
"div-series" together with the result of Exercise 3.59 to
generate the power series for tangent.

|#

#| Answer |#

(load-ex "3.61")

(define (div-series n d)
  (let* ([c (stream-car d)]
         [scaled-n (scale-stream n (/ 1 c))]
         [scaled-d (scale-stream d (/ 1 c))])
    (if (= 0 c)
        (error "div-series" "denominator has a zero constant term" c)
        (mul-series scaled-n (invert-unit-series scaled-d)))))

(define tangent-series (div-series sine-series cosine-series))

#| Tests

I used the following to examine many numbers:

(big-test 0.0 3 0.01 tan tangent)

tan has asymptotes every Pi/2. This tan-series approximation works well up until
close to Pi/2 and then never recovers back to a useful value. For example, the
below was run with 250 terms of the tan-series computed per value.

at x : (tan . tan-series approx)
. . .
at 1.5200000000000011: (19.669527820559303 . 19.664305078032857)
at 1.5300000000000011: (24.49841044183872 . 24.46479679983056)
at 1.5400000000000011: (32.461138912857976 . 32.2334241583552)
at 1.5500000000000012: (48.07848247922164 . 46.37214504185126)
at 1.5600000000000012: (92.62049631671408 . 76.16147196816732)
at 1.5700000000000012: (1255.7655915025405 . 149.79802942990077)
at 1.5800000000000012: (-108.64920360482999 . 360.821010591731)
at 1.5900000000000012: (-52.066969650909336 . 1040.7769083800895)
. . .

I assume understanding why this happens is beyond the scope of the exercise.

|#

;;; computes the power-series for a given x for terms-count terms.
(define (compute-series s x terms-count)
  (define (iter result s x exponent)
    (if (= exponent terms-count)
        result
        (let* ([constant-part (stream-car s)]
               [x-part (expt x exponent)]
               [term (* constant-part x-part)])
          (iter (+ result term) (stream-cdr s) x (+ exponent 1)))))
  (iter 0 s x 0))

(define (sine x) (compute-series sine-series x 100))
(define (cosine x) (compute-series cosine-series x 100))
(define (tangent x) (compute-series tangent-series x 100))
       
(define (approximates? x y)
  (< (abs (- x y)) 0.00001))

(define (big-test start stop increment f g)
  (define (iter x)
    (if (> x stop)
        (void)
        (let ([f-result (f x)]
              [g-result (g x)])
          (if (not (approximates? f-result g-result))
              (begin
                (newline)
                (display (format "at ~a: (~a . ~a)" 
                                 x 
                                 f-result 
                                 g-result)))
              (void))
          (iter (+ x increment)))))
  (iter start))

(define-test (map (lambda (i) (stream-ref tangent-series i)) (iota 8))
             (list 0 1 0 1/3 0 2/15 0 17/315))
(define-test (approximates? (sine 0.0) (sin 0.0)) #t)
(define-test (approximates? (cosine 0.0) (cos 0.0)) #t)
(define-test (approximates? (tangent 0.0) (tan 0.0)) #t)
(define-test (approximates? (sine 0.5) (sin 0.5)) #t)
(define-test (approximates? (cosine 0.5) (cos 0.5)) #t)
(define-test (approximates? (tangent 0.5) (tan 0.5)) #t)
(define-test (approximates? (sine 1.0) (sin 1.0)) #t)
(define-test (approximates? (cosine 1.0) (cos 1.0)) #t)
(define-test (approximates? (tangent 1.0) (tan 1.0)) #t)