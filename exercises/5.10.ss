#|

Exercise 5.10: Design a new syntax for register-machine
instructions and modify the simulator to use your new
syntax. Can you implement your new syntax without changing
any part of the simulator except the syntax procedures in
this section?

|#

(load-ex "5.9")

#| Answer 

Add inc and dec.

|#

(define make-execution-procedure-51 make-execution-procedure)
(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond [(eq? (car inst) 'inc)
         (make-inc inst machine pc 1)]
        [(eq? (car inst) 'dec)
         (make-inc inst machine pc -1)]
        [else 
         (make-execution-procedure-51 inst labels machine pc flag stack ops)]))

(define (make-inc inst machine pc amount)
  (let ([target (get-register machine (assign-reg-name inst))])
    (lambda ()
      (set-contents! target (+ (get-contents target) amount))
      (advance-pc pc))))

#| Tests |#
(define (510-m) (make-machine '(a b) (list) '((inc a) (dec b))))

(define-test (let ([m (510-m)]) (begin
  (set-register-contents! m 'a 10)
  (set-register-contents! m 'b 10)
  (start m)
  (list
    (get-register-contents m 'a)
    (get-register-contents m 'b))))
  (list 11 9))
