#|

Exercise 3.82: Redo Exercise 3.5 on Monte Carlo integration
in terms of streams. The stream version of
"estimate-integral" will not have an argument telling how
many trials to perform. Instead, it will produce a stream of
estimates based on successively more trials.

|#

(load-ex "3.81")

#| Answer |#
(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (random range))))

;;; stream of (trials-passed . trials).
;;; the first element is 0 -- aka no trials run yet
;;; the nth element is the result after n trials.
(define (monte-carlo-stream experiment)
  (define (iter trials trials-passed)
    (cons-stream 
      (if (= trials 0) 0 (/ trials-passed trials))
      (let ([passed (experiment)])
        (iter (+ trials 1) (if passed
                               (+ trials-passed 1)
                               trials-passed)))))
  (iter 0 0))

(define (estimated-integral-stream P x1 x2 y1 y2)
  (monte-carlo-stream (lambda ()
        (let ([x (random-in-range x1 x2)]
          [y (random-in-range y1 y2)])
      (P x y)))))

(define (estimate-pi-using-unit-circle-stream)
  (define (in-unit-circle x y)
    (<= (+ (expt (- x 1.0) 2.0)
           (expt (- y 1.0) 2.0))
        1))
  (scale-stream (estimated-integral-stream in-unit-circle 0.0 2.0 0.0 2.0) 4.0))

#| Tests 

;;; same estimate as test in 3.5
> (stream-ref (estimate-pi-using-unit-circle-stream) 10000000)
3.1417528 ; real value is 3.14159

|#