#|

Exercise 3.7: Consider the bank account objects created by
"make-account", with the password modification described in
Exercise 3.3. Suppose that our banking system requires the
ability to make joint accounts. Define a procedure
"make-joint" that accomplishes this. "make-joint" should
take three arguments. The first is a password-protected
account. The second argument must match the password with
which the account was defined in order for the "make-joint"
operation to proceed. The third argument is a new password.
"make-joint" is to create an additional access to the
original account using the new password. For example, if
"peter-acc" is a bank account with password "open-sesame",
then

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

will allow one to make transactions on "peter-acc" using the
name "paul-acc" and the password "rosebud". You may wish to
modify your solution to Exercise 3.3 to accommodate this new
feature.

|#

(load-ex "3.3")

(define (make-joint acc acc-pw new-pw)
  (lambda (pw m)
    (if (eq? pw new-pw)
        (lambda (x) ((acc acc-pw m) x))
        (lambda (x) "Incorrect password"))))

#| Tests |#
(define-test (begin
               (define a (make-account 100 'a))
               (define b (make-joint a 'a 'b))
               ((a 'a 'withdraw) 10)
               ((b 'b 'withdraw) 10))
             80)
