#|

Exercise 5.6: Ben Bitdiddle observes that the Fibonacci
machine's controller sequence has an extra "save" and an
extra "restore", which can be removed to make a faster
machine. Where are these instructions?

|#

(load-ex "5.4")

#| Code from book -- Figure 5.12 -- Controller for a machine to compute Fibonacci numbers. |#

#| 01: |# (define (fib-machine) (make-machine '(continue n val) (list (list '< <) (list '- -) (list '+ +))
#| 02: |#   '((assign continue (label fib-done))
#| 03: |#     fib-loop
#| 04: |#       (test (op <) (reg n) (const 2))
#| 05: |#       (branch (label immediate-answer))
#| 06: |#       ;; set up to compute Fib(n - 1)
#| 07: |#       (save continue)
#| 08: |#       (assign continue (label afterfib-n-1))
#| 09: |#       (save n)                              ; save old value of n
#| 10: |#       (assign n (op -) (reg n) (const 1))   ; clobber n to n - 1
#| 11: |#       (goto (label fib-loop))               ; perform recursive call
#| 12: |#     afterfib-n-1                            ; upon return, val contains Fib(n - 1)
#| 13: |#       (restore n)
#| 14: |#       ; (restore continue)
#| 15: |#       ;; set up to compute Fib(n - 2)
#| 16: |#       (assign n (op -) (reg n) (const 2))
#| 17: |#       ; (save continue)
#| 18: |#       (assign continue (label afterfib-n-2))
#| 19: |#       (save val)                            ; save Fib(n - 1)
#| 20: |#       (goto (label fib-loop))
#| 21: |#     afterfib-n-2                            ; upon return, val contains Fib(n - 2)
#| 22: |#       (assign n (reg val))                  ; n now contains Fib(n - 2)
#| 23: |#       (restore val)                         ; val now contains Fib(n - 1)
#| 24: |#       (restore continue)
#| 25: |#       (assign val (op +) (reg val) (reg n)) ;  Fib(n - 1) +  Fib(n - 2)
#| 26: |#       (goto (reg continue))                 ; return to caller, answer is in val
#| 27: |#     immediate-answer
#| 28: |#       (assign val (reg n))                  ; base case:  Fib(n) = n
#| 29: |#       (goto (reg continue))
#| 30: |#     fib-done)))

#| Answer 

Lines 14 and 17 above can be removed. The "continue" value is restored and saved
again without being used.

|#

#| Tests |#
(define-test (let ([m (fib-machine)]) (begin
  (set-register-contents! m 'n 9)
  (start m)
  (get-register-contents m 'val)))
  34)
