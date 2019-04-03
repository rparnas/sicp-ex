#|

Exercise 1.39: A continued fraction representation of the
tangent function was published in 1770 by the German
mathematician J.H. Lambert:

              x
tan x = ---------------
                x^2
        1 - -----------
                  x^2
            3 - -------
                5 - ...

where x is in radians. Define a procedure "(tan-cf x k)"
that computes an approximation to the tangent function based
on Lambert's formula. "k" specifies the number of terms to
compute, as in Exercise 1.37.

|#

#| Answer |#

(load-ex "1.37")

(define (tan x k)
  (count-frac
   (lambda (i) (if (= i 1) x (- (square x))))
   (lambda (i) (- (* i 2.0) 1.0))
   k))

#| Tests -- manual

> (tan 25 100)
-0.1335264070215359 ; answer is -0.133526407021535879998...

|#

