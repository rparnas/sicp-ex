#|

Exercise 1.38: In 1737, the Swiss mathematician Leonhard
Euler published a memoir De Fractionibus Continuis, which
included a continued fraction expansion for e - 2, where e
is the base of the natural logarithms. In this fraction, the
N_i are all 1, and the D_i are successively 1, 2, 1, 1, 4,
1, 1, 6, 1, 1, 8,.... Write a program that uses your
"cont-frac" procedure from Exercise 1.37 to approximate e,
based on Euler's expansion.

|#

(load-ex "1.37")

(define (e-euler k) 
  (+ 2.0 (count-frac (lambda (i) 1)
         (lambda (i)
           (if (= (remainder i 3) 2)
         (/ (+ i 1) 1.5)
         1))
         k)))

#| Tests 

> (e-euler 100)
  2.7182818284590455 ; answer is 2.7182818284590452...

|#