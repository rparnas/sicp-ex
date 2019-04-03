#|

Exercise 2.83: Suppose you are designing a generic
arithmetic system for dealing with the tower of types shown
in Figure 2.25: integer, rational, real, complex. For each
type (except complex), design a procedure that raises
objects of that type one level in the tower. Show how to
install a generic "raise" operation that will work for each
type (except complex).

|#

(load-ex "2.82")

#| Answer -- Packages for new types|#
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
    (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
    (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer)
    (lambda (x y) (= x y)))
  (put '=zero? '(integer)
    (lambda (x) (= 0 x)))
  (put 'make 'integer (lambda (x) (tag x)))
  'done)
(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
    (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
    (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real)
    (lambda (x y) (= x y)))
  (put '=zero? '(real)
    (lambda (x) (= 0 x)))
  (put 'make 'real (lambda (x) (tag x)))
  'done)
(define (make-real n)
  ((get 'make 'real) n))

#| Answer -- coercion and raise |#

; TODO: How should one know they can use contents directly?
(put-coercion 
  'integer 
  'rational 
  (lambda (x) (make-rational (contents x) 1)))

(put-coercion 
  'rational 
  'real 
  (lambda (x)
    ;; Hack -- copied from 2.77 -- interal knowledge of rational
    (define (numer x) (cadr x))
    (define (denom x) (cddr x))
    ;; ========================
    (make-real (/ (numer x) (denom x)))))

;; TODO: Has internal knowledge of coercion
(put-coercion 
  'real 
  'complex 
  (lambda (x) (make-complex-from-real-imag (cdr x) 0)))

(define tower (list 'integer 'rational 'real 'complex))

(define (raise x)
  (let* ([type (type-tag x)]
         [pos (memq type tower)])
    (if (< (length pos) 2)
        (error 'raise "can't raise" type)
        ((get-coercion (car pos) (cadr pos)) x))))

(install-integer-package)
(install-real-package)

#| Tests |#
(define-test (make-integer 5) 
             '(integer . 5))
(define-test (raise (make-integer 5)) 
             '(rational 5 . 1))
(define-test (raise (raise (make-integer 5)))
             '(real . 5))
(define-test (raise (raise (raise (make-integer 5))))
             '(complex rectangular 5 . 0))
(define-test (raise (make-rational 2 7))
             '(real . 2/7))
(define-test (raise (raise (make-rational 2 7))) 
             '(complex rectangular 2/7 . 0))