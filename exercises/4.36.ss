#|

Exercise 4.36: Exercise 3.69 discussed how to generate the
stream of all Pythagorean triples, with no upper bound on
the size of the integers to be searched. Explain why simply
replacing "an-integer-between" by "an-integer-starting-from"
in the procedure in Exercise 4.35 is not an adequate way to
generate arbitrary Pythagorean triples. Write a procedure
that actually will accomplish this. (That is, write a
procedure for which repeatedly typing "try-again" would in
principle eventually generate all Pythagorean triples.)

|#

(load-ex "4.35")

#| Answer 

The try-again system re-tries the calculation by varying the most recent
ambigous value. If that most recent ambigous value has infinite possibilities
all other ambigous values remain frozen as those possibilities are explored. For
pythogrean-triples we count up k, leaving i and j at one. This never generates a
triple as there is no triple in the form (1 1 k).

When searching, only the first ambigous value can have infinite possibilities to
avoid some values never having thier posibilities tried.

The current system forces the user to explicitly define their search strategy.
The system could be changed to vary which amb values are varied. However if the
user cares about the order of values returned from an infinite set they must
understand how the underlying amb searches, so it is best to keep that search
simple as it is now.

|#

(define setup-environment-435 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-435)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))
    ;;; correct answer
    (add '(define (all-pythagorean-triples)
            (let* ([k (an-integer-starting-from 1)]
                   [j (an-integer-between 1 k)]
                   [i (an-integer-between 1 j)])
              (require (= (+ (* i i) (* j j)) (* k k)))
                (list i j k))))
    env))

#| Tests |#
(define-test (eval-n '(all-pythagorean-triples) 6)
             '((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17) (12 16 20)))