#|

Exercise 5.11: When we introduced "save" and "restore" in
Section 5.1.4, we didn't specify what would happen if you
tried to restore a register that was not the last one saved,
as in the sequence

(save y)  (save x)  (restore y)

There are several reasonable possibilities for the meaning
of "restore":

a. "(restore y)" puts into "y" the last value saved on the
stack, regardless of what register that value came from.
This is the way our simulator behaves. Show how to take
advantage of this behavior to eliminate one instruction from
the Fibonacci machine of Section 5.1.4 (Figure 5.12).

b. "(restore y)" puts into "y" the last value saved on the
stack, but only if that value was saved from "y"; otherwise,
it signals an error. Modify the simulator to behave this
way. You will have to change "save" to put the register name
on the stack along with the value.

c. "(restore y)" puts into "y" the last value saved from "y"
regardless of what other registers were saved after "y" and
not restored. Modify the simulator to behave this way. You
will have to associate a separate stack with each register.
You should make the "initialize-stack" operation initialize
all the register stacks.

|#

(load-ex "5.10")

#| Awnser -- a.

(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; save old value of n
   (assign n (op -) (reg n) (const 1)); clobber n to n - 1
   (goto (label fib-loop))            ; perform recursive call
 afterfib-n-1                         ; upon return, val contains Fib(n - 1)
   (restore n)
   (restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   (assign n (reg val))               ; n now contains Fib(n - 2)               ; delete -- val still has Fib(n-2)
   (restore val)                      ; val now contains Fib(n - 1)             ; replace w/ (restore n) -- n now contains Fib(n-1)
   (restore continue)
   (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
           (op +) (reg val) (reg n)) 
   (goto (reg continue))              ; return to caller, answer is in val
 immediate-answer
   (assign val (reg n))               ; base case:  Fib(n) = n
   (goto (reg continue))
 fib-done)

|#

#| Answer -- b. |#
(define (make-save inst machine stack pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)])
    (lambda ()
      (let ([x (cons reg-name (get-contents reg))])
        (push stack x))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)])
    (lambda ()
      (let ([x (pop stack)])
        (if (eq? (car x) reg-name)
            (set-contents! reg (cdr x))
            (error "machine" (format "tried to restore '~a' with value from '~a'" 
                              reg-name 
                              (car x)))))
      (advance-pc pc))))

#| Tests -- b. 

> (define 511-m (make-machine '(a b) (list)'((save a) (restore b))))
> (start 511-m)
Exception in machine: tried to restore 'b' with value from 'a'
Type (debug) to enter the debugger.

|#

#| Answer -- c. (not carried forward)

(define (make-register name)
  (let ([contents '*unassigned*] [s (make-stack)])
    (define (dispatch message)
      (cond [(eq? message 'get) 
             contents]
            [(eq? message 'set)
             (lambda (value) (set! contents value))]
            [(eq? message 'pop)
             (lambda () (set! contents (s 'pop)))]
            [(eq? message 'push)
             (lambda () ((s 'push) contents))]
            [else
             (error "make-register" "unknown request" message)]))
    dispatch))

(define (make-save inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      ((reg 'push))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      ((reg 'pop))
      (advance-pc pc))))

(define (511-m) (make-machine '(a b) (list) '(
  (assign a (const 5))
  (assign b (const 6))
  (save a)
  (save b)
  (assign a (const 0))
  (assign b (const 0))
  (restore a)
  (restore b))))

(let ([m (511-m)])
  (begin (start 511-m) (list (get-register-contents 511-m 'a)
                             (get-register-contents 511-m 'b))))

; outputs (5 6) 

|#