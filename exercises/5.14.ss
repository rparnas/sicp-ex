#|

Exercise 5.14: Measure the number of pushes and the maximum
stack depth required to compute n! for various small values
of n using the factorial machine shown in Figure 5.11. From
your data determine formulas in terms of n for the total
number of push operations and the maximum stack depth used
in computing n! for any n > 1. Note that each of these is a
linear function of n and is thus determined by two
constants. In order to get the statistics printed, you will
have to augment the factorial machine with instructions to
initialize the stack and print the statistics. You may want
to also modify the machine so that it repeatedly reads a
value for n, computes the factorial, and prints the result
(as we did for the GCD machine in Figure 5.4), so that you
will not have to repeatedly invoke "get-register-contents",
"set-register-contents!", and "start".

|#

(load-ex "5.13")

#| Code from book -- stack diagnostics |#

;;; patch of 5.13
(define make-new-machine-513 make-new-machine)
(define (make-new-machine)
  (let ([m (make-new-machine-513)])
    ((m 'install-operations)
     (list (list 'initialize-stack
                 (lambda () (stack 'initialize)))
           (list 'print-stack-statistics
                 (lambda () (stack 'print-statistics)))))
    m))

;;; replace 5.1 -- modified to return rather than display stats.
(define (make-stack)
  (let ([s '()]
        [number-pushes 0]
        [max-depth 0]
        [current-depth 0])
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "pop" "empty stack")
          (let ([top (car s)])
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (get-statistics)
      (format "pushes=~a, max-depth=~a" number-pushes max-depth))
    (define (dispatch message)
      (cond [(eq? message 'push) push]
            [(eq? message 'pop) (pop)]
            [(eq? message 'initialize) (initialize)]
            [(eq? message 'get-statistics) (get-statistics)]
            [else
             (error "stack" "unknown request" message)]))
    dispatch))

;;; figure 5.11 -- recursive factorial machine
(define (factorial-recursive-machine)
  (make-machine 
    (list (list '= =) 
          (list '- -) 
          (list '* *))
    '((assign continue (label fact-done))     ; set up final return address
      fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        ;; Set up for the recursive call by saving n and continue.
        ;; Set up continue so that the computation will continue
        ;; at after-fact when the subroutine returns.
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
      after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
        (goto (reg continue))                   ; return to caller
      base-case
        (assign val (const 1))                  ; base case: 1! = 1
        (goto (reg continue))                   ; return to caller
      fact-done)))

#| Answer 

Stats for the recursive factorial machine from figure 5.11 for n=1..5

  (do-factorial-test 1) => (1        . "pushes=0, max-depth=0")
  (do-factorial-test 2) => (2        . "pushes=2, max-depth=2")
  (do-factorial-test 3) => (6        . "pushes=4, max-depth=4")
  (do-factorial-test 4) => (24       . "pushes=6, max-depth=6")
  (do-factorial-test 5) => (120      . "pushes=8, max-depth=8")
  (do-factorial-test 6) => (720      . "pushes=10, max-depth=10")
  (do-factorial-test 7) => (5040     . "pushes=12, max-depth=12")
  (do-factorial-test 8) => (40320    . "pushes=14, max-depth=14")
  (do-factorial-test 9) => (362880   . "pushes=16, max-depth=16")
  (do-factorial-test 10) => (3628800 . "pushes=18, max-depth=18")

Both pushes and max-depth is 2*(n-1).

|#

#| Tests |#
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (do-factorial-test n)
  (let ([m (factorial-recursive-machine)])
    (begin
      (set-register-contents! m 'n n)
      (start m)
      (cons (get-register-contents m 'val)
            ((m 'stack) 'get-statistics)))))

(define-test (do-factorial-test 12)
             (cons (factorial 12) "pushes=22, max-depth=22"))