#|

Exercise 2.96:

a. Implement the procedure "pseudoremainder-terms", which is
just like "remainder-terms" except that it multiplies the
dividend by the integerizing factor described above before
calling "div-terms". Modify "gcd-terms" to use
"pseudoremainder-terms", and verify that
"greatest-common-divisor" now produces an answer with
integer coefficients on the example in Exercise 2.95.

b. The GCD now has integer coefficients, but they are larger
than those of P_1. Modify "gcd-terms" so that it removes
common factors from the coefficients of the answer by
dividing all the coefficients by their (integer) greatest
common divisor.

|#

