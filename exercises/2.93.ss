#|

Exercise 2.93: Modify the rational-arithmetic package to use
generic operations, but change "make-rat" so that it does
not attempt to reduce fractions to lowest terms. Test your
system by calling "make-rational" on two polynomials to
produce a rational function:

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

Now add "rf" to itself, using "add". You will observe that
this addition procedure does not reduce fractions to lowest
terms.

|#

(load-ex "2.92")

#| Answer -- rewrites from 2.86, 2.85, 2.77 |#
(define (upgrade-rational)
  ; --selectors--
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ; --constructors--
  (define (make-rat n d)
    (if (not (find (lambda (x) (eq? x (type-tag n)))
                   '(integer polynomial)))
        (error "make-rational" "n type not supported" n))
    (if (not (find (lambda (x) (eq? x (type-tag d)))
                   '(integer polynomial)))
        (error "make-rational" "d type not supported" d))
    (attach-tag 'rational (cons n d)))

  ; --operations--
  (define (add-rat x y)
    (make-rational (add (mul (numer x) (denom y))
                        (mul (numer y) (denom x)))
                   (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rational (sub (mul (numer x) (denom y))
                        (mul (numer y) (denom x)))
                   (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rational (mul (numer x) (numer y))
                   (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rational (mul (numer x) (denom y))
                   (mul (denom x) (numer y))))

  ; --interface--
  (put 'make 'rational (lambda (n d) (make-rat n d)))
  (put 'add '(rational rational) (lambda (x y) (add-rat x y)))
  (put 'sub '(rational rational) (lambda (x y) (sub-rat x y)))
  (put 'mul '(rational rational) (lambda (x y) (mul-rat x y)))
  (put 'div '(rational rational) (lambda (x y) (div-rat x y)))
  ; --idk--
  (put 'div '(integer integer) (lambda (x y) (make-rational (make-integer x) (make-integer y))))
  (put 'project '(rational) (lambda (x)
    (let ([n (numer x)]
          [d (denom x)])
      (if (and (is-type? n 'integer) (is-type? d 'integer))
          (make-integer (round (/ (contents n) (contents d))))
          (car (div n d)))))) ; polynomial division
  (put 'negate '(integer) (lambda (x)
    (make-integer (- x))))
  (put '=zero? '(rational)
    (lambda (x) (equ? (numer x) (make-integer 0))))
  (put 'negate '(rational) (lambda (x)
    (make-rational (negate (numer x))
                   (denom x))))
  (void))

(put-coercion 
  'integer 
  'rational 
  (lambda (x) (make-rational x (make-integer 1))))

(upgrade-rational)

(define (is-type? x type)
  (equal? type (type-tag x)))

; disable automatic simplification from 2.85
; Need to reevaluate tower of types because there
; are so many nested types now.
(define apply-generic old-apply-generic)

#| Tests |#

(define-test (equ? (make-rational (make-integer 2) (make-integer 1))
                   (make-integer 2))
             #t)
(define-test (negate (make-integer 2))
             '(integer . -2))
(define-test (negate (make-rational (make-integer -1) (make-integer 1)))
             '(rational (integer . 1) integer . 1))

; (x^3 + 1) / (x^2 + 1)
(define p1 (make-polynomial (make-sparse-termlist 
  (make-term "x^2" (make-integer 1))
  (make-term "" (make-integer 1)))))
(define p2 (make-polynomial (make-sparse-termlist 
  (make-term "x^3" (make-integer 1))
  (make-term "" (make-integer 1)))))
(define rf (make-rational p2 p1))

(define-test rf 
             '(rational
                (polynomial sparse-termlist
                  (term "x^3" (integer . 1))
                  (term "" (integer . 1)))
                 polynomial sparse-termlist
                  (term "x^2" (integer . 1))
                  (term "" (integer . 1))))

; in: [(x^3 + 1)/(x^2 + 1)] + [(x^3 + 1)/(x^2 + 1)]
; out: (2x^5 + 2x^3 + 2x^2 + 2) / (x^4 + 2x^2 + 1)
; desired: (2x^3 + 2) / (x^2 + 1)
; these are equivalent

(define-test (add rf rf)
             '(rational
                (polynomial sparse-termlist
                  (term "x^5" (integer . 2))
                  (term "x^3" (integer . 2))
                  (term "x^2" (integer . 2))
                  (term "" (integer . 2)))
                 polynomial sparse-termlist
                  (term "x^4" (integer . 1))
                  (term "x^2" (integer . 2))
                  (term "" (integer . 1))))

#| Notes

; I don't understand how this code is supposed to interact
with the tower of types code.

; Equivalence does not work

> (define desired (make-rational (make-polynomial (make-sparse-termlist (make-term "x^3" (make-integer 2))
                                                                        (make-term "" (make-integer 2))))
                                 (make-polynomial (make-sparse-termlist (make-term "x^2" (make-integer 1))
                                                                        (make-term "" (make-integer 1))))))
> desired
(rational
  (polynomial
    sparse-termlist
    (term "x^3" (integer . 2))
    (term "" (integer . 2)))
  polynomial sparse-termlist (term "x^2" (integer . 1))
  (term "" (integer . 1)))
> (eqv? desired (add rf rf))
#f

|#
