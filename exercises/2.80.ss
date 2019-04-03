#|

Exercise 2.80: Define a generic predicate "=zero?" that
tests if its argument is zero, and install it in the generic
arithmetic package. This operation should work for ordinary
numbers, rational numbers, and complex numbers.

|#

#| Answer |#
(load-ex "2.79")

(define (=zero? x)
  (apply-generic '=zero? x))

(define (upgrade-scheme-number-package-=zero?)
  (put '=zero? '(scheme-number)
    (lambda (x) (equal? x 0)))
  'done)

(define (upgrade-rational-package-=zero?)
  ;; Hack -- coupled to 2.73 implementation
  (define (numer z) (car z))
  ;; ======================================
  (put '=zero? '(rational)
    (lambda (x) (equ? (numer x) 0)))
  'done)

(define (upgrade-complex-package-=zero?)
  (put '=zero? '(complex)
    (lambda (x)
      (and (equ? (real-part x) 0)
           (equ? (imag-part x) 0))))
  'done)

(upgrade-scheme-number-package-=zero?)
(upgrade-rational-package-=zero?)
(upgrade-complex-package-=zero?)

#| Tests |#
(define-test (=zero? (sub (make-scheme-number 1) (make-scheme-number 1))) #t)
(define-test (=zero? (sub (make-rational 1 2) (make-rational 2 4))) #t)
(define-test (=zero? (sub (make-rational 1 2) (make-rational 2 5))) #f)
(define-test (=zero? (sub (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 1 2))) #t)
