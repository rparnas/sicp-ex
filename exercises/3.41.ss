#|

Exercise 3.41: Ben Bitdiddle worries that it would be better
to implement the bank account as follows (where the
commented line has been changed):

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected
               (lambda () balance))))  ; serialized 
            (else
             (error "Unknown request: MAKE-ACCOUNT"
                    m))))
    dispatch))

because allowing unserialized access to the bank balance can
result in anomalous behavior. Do you agree? Is there any
scenario that demonstrates Ben's concern?

|#

#| Answer 

If deposit/withdraw is called at the same time as balance, the user invoking
balance gets one of two values: the valid value before the deposit/withdraw is
applied or the valid value after the deposit/withdraw is applied.

This means the user will sometimes get a slightly stale value. However, I don't
view this as necessaitating protection of the balance function. If you call
balance 1 millisecond before the withdraw you get a stale value, so why would
calling at the same time of the withdraw and still getting a stale value be a
big deal? You have to make a value judgement as to whether you want "a valid
value promptly" or you want to wait for all deposit/withdraw requests ahead of
your balance requests to finish first.

However protection is a good idea architecturally. Leaving out protection
assumes that the balance variable will never be in an invalid state while
deposit/withdraw actions are in progress. Balance shouldn't need to know this
internal detail about deposit/withdraw. Similarly, withdraw/deposit shouldn't
need to know that the internal details of the balance action do not affect the
balance variable.

Analysis:
  Actions in withdraw
    a=balance                   ; action 0 -- for (if (>= balance...
    b=balance                   ; action 1
    (set! balance (- b amount)) ; action 2
    c=balance                   ; action 3 -- for return

  Actions in deposit
    a=balance              ; action 4
    balance = (+ a amount) ; action 5
    b=balance              ; action 6

  Actions in balance
    a=balance ; action 7 -- for return
|#