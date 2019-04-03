#|

Exercise 3.81: Exercise 3.6 discussed generalizing the
random-number generator to allow one to reset the
random-number sequence so as to produce repeatable sequences
of "random" numbers. Produce a stream formulation of this
same generator that operates on an input stream of requests
to "generate" a new random number or to "reset" the sequence
to a specified value and that produces the desired stream of
random numbers. Don't use assignment in your solution.

|#

(load-ex "3.80")

#| Answer |#
(define (rand-stream-map requests)
  (define reset-seed 7)
  (define (rand-update x)
    (let ([a 109]
          [b 17]
          [m 19])
      (mod (+ (* a x) b) m)))
  (define (iter seed requests)
    (let ([arg (stream-car requests)])
      (cond [(eq? arg 'generate)
             (let ([new-seed (rand-update seed)])
               (cons-stream new-seed
                            (iter new-seed (stream-cdr requests))))]
            [(eq? arg 'reset)
             (cons-stream "reset"
                          (iter reset-seed (stream-cdr requests)))] 
            [else
             (error "rand" "no such method")])))
  (iter reset-seed requests))

#| Tests |#

;;; same as test for 3.6
(define-test (let* ([r-list '(reset generate generate generate generate generate
                              reset generate generate generate generate generate)]
                    [requests (list->stream r-list)]
                    [rand (rand-stream-map requests)])
               (map (lambda (i) (stream-ref rand i)) (iota (length r-list))))
            '("reset" 1 12 14 4 16 "reset" 1 12 14 4 16))
