#|

Exercise 1.27: Demonstrate that the Carmichael numbers
listed in Footnote 1.47 really do fool the Fermat test. That
is, write a procedure that takes an integer n and tests
whether a^n is congruent to a modulo n for every a < n, and
try your procedure on the given Carmichael numbers.

|#

#| Answer |#
(load-ex "1.24")

#| Yes, they do. |#

#| Tests |#
(define-test (fast-prime? 561 5) #t)
