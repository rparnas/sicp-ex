#|

Exercise 2.95: Define P_1, P_2, and P_3 to be the
polynomials

P_1 : x^2 - 2x + 1

P_2 : 11x^2 + 7

P_3 : 13x + 5

Now define Q_1 to be the product of P_1 and P_2 and Q_2 to
be the product of P_1 and P_3, and use
"greatest-common-divisor" (Exercise 2.94) to compute the GCD
of Q_1 and Q_2. Note that the answer is not the same as P_1.
This example introduces noninteger operations into the
computation, causing difficulties with the GCD algorithm. To
understand what is happening, try tracing "gcd-terms" while
computing the GCD or try performing the division by hand.

|#

(load-ex "2.94")

#| Answer |#

; x^2 - 2x + 1
(define P1 (make-polynomial (make-sparse-termlist
  (make-term "x^2" (make-integer 1))
  (make-term "x" (make-integer -2))
  (make-term "" (make-integer 1)))))

; 11x^2 + 7
(define P2 (make-polynomial (make-sparse-termlist
  (make-term "x^2" (make-integer 11))
  (make-term "" (make-integer 7)))))

; 13x + 5
(define P3 (make-polynomial (make-sparse-termlist
  (make-term "x" (make-integer 13))
  (make-term "" (make-integer 5)))))


(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))

#| Tests |#

; (x^2 - 2x + 1)(11x^2 + 7) = 11x^4 - 22x^3 + 18x^2 - 14x + 7
(define-test Q1 
             '(polynomial sparse-termlist 
                (term "x^4" (integer . 11))
                (term "x^3" (integer . -22))
                (term "x^2" (integer . 18))
                (term "x" (integer . -14))
                (term "" (integer . 7))))

; (x^2 - 2x + 1)(13x + 5) = 13x^3 - 21x^2 + 3x + 5
(define-test Q2
             '(polynomial sparse-termlist
                (term "x^3" (integer . 13))
                (term "x^2" (integer . -21))
                (term "x" (integer . 3))
                (term "" (integer . 5))))

; GCD[11x^4 - 22x^3 + 18x^2 - 14x + 7, 13x^3 - 21x^2 + 3x + 5]
(define-test (greatest-common-divisor Q1 Q2)
             '(polynomial sparse-termlist
                (term "x^2" (rational (integer . 18954) integer . 2197))
                (term "x" (rational (integer . -37908) integer . 2197))
                (term "" (rational (integer . 1458) integer . 169))))

#| Notes

Fractions are introduced when computing the very first
remainder needed by gcd. In doing this remainder on the
first step you divide the highest term over the highest
term.

; GCD
GCD[11x^4 - 22x^3 + 18x^2 - 14x + 7, 13x^3 - 21x^2 + 3x + 5]
GCD[13x^3 - 21x^2 + 3x + 5, Remainder of (11x^4 - 22x^3 + 18x^2 - 14x + 7) / (13x^3 - 21x^2 + 3x + 5)]

; (11x^4 - 22x^3 + 18x^2 - 14x + 7) / (13x^3 - 21x^2 + 3x + 5)
; (1) | 11x^4 / 13x^3 = (11/13)x

|#