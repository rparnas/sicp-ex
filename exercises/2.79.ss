#|

Exercise 2.79: Define a generic equality predicate "equ?"
that tests the equality of two numbers, and install it in
the generic arithmetic package. This operation should work
for ordinary numbers, rational numbers, and complex numbers.

|#

#| Answer |#
(load-ex "2.78")

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (upgrade-scheme-number-package-equ?)
  (put 'equ? '(scheme-number scheme-number) 
    (lambda (x y) (= x y)))
  'done)

(define (upgrade-rational-package-equ?)
  ;; Hack -- coupled to 2.73 implementation
  (define (numer z) (car z))
  (define (denom z) (cdr z))
  ;; ======================================
  (put 'equ? '(rational rational)
    (lambda (x y)
      (and (equ? (numer x) (numer y))
           (equ? (denom x) (denom y)))))
  'done)

(define (upgrade-complex-package-equ?)
  (put 'equ? '(complex complex)
    (lambda (x y)
      (and (equ? (real-part x) (real-part y))
           (equ? (imag-part x) (imag-part y)))))
  'done)

(upgrade-scheme-number-package-equ?)
(upgrade-rational-package-equ?)
(upgrade-complex-package-equ?)

#| Tests |#

(define-test (equ? (make-scheme-number 5) (make-scheme-number 5)) #t)
(define-test (equ? (make-scheme-number 5) (make-scheme-number 6)) #f)
(define-test (equ? 1 1.0) #t)

(define-test (equ? (make-rational 1 2) (make-rational 2 4)) #t)
(define-test (equ? (make-rational 2 3) (make-rational 0 7)) #f)

(define-test (equ? (make-complex-from-real-imag 2 1) (make-complex-from-real-imag 2 1)) #t)
(define-test (equ? (make-complex-from-real-imag 0 0) (make-complex-from-mag-ang 0 0)) #t)

;; Note: There are floating-point rounding errors beyond the scope of this exercise
; > (define a (make-complex-from-real-imag 2 1))
; > (define b (make-complex-from-mag-ang (magnitude a) (angle a)))
; > (equ? a b)
; #f
