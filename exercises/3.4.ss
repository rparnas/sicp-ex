#|

Exercise 3.4: Modify the "make-account" procedure of
Exercise 3.3 by adding another local state variable so that,
if an account is accessed more than seven consecutive times
with an incorrect password, it invokes the procedure
"call-the-cops".

|#

#| Answer |#
(define (call-the-cops)
  (display "calling the cops!"))

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
  (let ([bad-password-count 0])
    (lambda (pw m)
      (if (eq? pw password)
          (begin
            (set! bad-password-count 0)
            (dispatch m))
          (begin
            (set! bad-password-count (+ bad-password-count 1))
            (if (> bad-password-count 7)
                (call-the-cops)
                (void))
            (lambda (x) "Incorrect password"))))))

#| Tests -- manual 

> (define acc (make-account 100 'pw))
> ((acc 'pw 'withdraw) 40)
60
> ((acc 'x 'withdraw) 40)
"Incorrect password"
> ((acc 'x 'withdraw) 40)
"Incorrect password"
> ((acc 'x 'withdraw) 40)
"Incorrect password"
> ((acc 'x 'withdraw) 40)
"Incorrect password"
> ((acc 'x 'withdraw) 40)
"Incorrect password"
> ((acc 'x 'withdraw) 40)
"Incorrect password"
> ((acc 'x 'withdraw) 40)
"Incorrect password"
> ((acc 'x 'withdraw) 40)
calling the cops!"Incorrect password"

|#
