#|

Exercise 2.57: Extend the differentiation program to handle
sums and products of arbitrary numbers of (two or more)
terms. Then the last example above could be expressed as

(deriv '(* x y (+ x 3)) 'x)

Try to do this by changing only the representation for sums
and products, without changing the "deriv" procedure at all.
For example, the "addend" of a sum would be the first term,
and the "augend" would be the sum of the rest of the terms.

|#

#| Answer |#
(load-ex "2.56")

; This seems okay because these procedures are already
; coupled to knowledge about the internal representation
; of sums and products.

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '* (cddr s))))

#| Tests |#

; [dx/x] ax^2 + bx + c = 2ax + b
(define-test (deriv '(+ (* a (** x 2)) (* b x) c) 'x) 
             '(+ (* a (* 2 x)) b))
