#|

Exercise 1.46: Several of the numerical methods described in
this chapter are instances of an extremely general
computational strategy known as iterative improvement.
Iterative improvement says that, to compute something, we
start with an initial guess for the answer, test if the
guess is good enough, and otherwise improve the guess and
continue the process using the improved guess as the new
guess. Write a procedure "iterative-improve" that takes two
procedures as arguments: a method for telling whether a
guess is good enough and a method for improving a guess.
"iterative-improve" should return as its value a procedure
that takes a guess as argument and keeps improving the guess
until it is good enough. Rewrite the "sqrt" procedure of
Section 1.1.7 and the "fixed-point" procedure of Section
1.3.3 in terms of "iterative-improve".

|#


#| Answer |#

(define (average x y) (/ (+ x y) 2))
(define tolerance 0.00001)

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess) guess (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess))))
   x))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) 
                        (< (abs (- (f guess) guess))
                           0.00001))
                      (lambda (guess) (f guess)))
   first-guess))

#| Tests |#

; real answer is 2.236067977...
(define-test (sqrt 5.0) 
             2.2360688956433634) 

; answer from 1.36 was 4.555536364911781
(define-test (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
             4.555530807938518)
