#|

Exercise 3.3: Modify the "make-account" procedure so that it
creates password-protected accounts. That is, "make-account"
should take a symbol as an additional argument, as in

(define acc (make-account 100 'secret-password))

The resulting account object should process a request only
if it is accompanied by the password with which the account
was created, and should otherwise return a complaint:

((acc 'secret-password 'withdraw) 40)
 60 
((acc 'some-other-password 'deposit) 50)
 "Incorrect password" 

|#

#| Answer |#
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "make-account" "unknown request" m)]))
  (lambda (pw m)
    (if (eq? pw password)
        (dispatch m)
        (lambda (x) "Incorrect password"))))

#| Tests |#
(define-test (begin
               (define acc (make-account 100 'secret-password))
               (list 
                 ((acc 'secret-password 'withdraw) 40)
                 ((acc 'some-other-password 'deposit) 50)))
               (list 
                 60
                 "Incorrect password"))