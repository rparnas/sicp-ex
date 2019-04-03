#|

Exercise 2.86: Suppose we want to handle complex numbers
whose real parts, imaginary parts, magnitudes, and angles
can be either ordinary numbers, rational numbers, or other
numbers we might wish to add to the system. Describe and
implement the changes to the system needed to accommodate
this. You will have to define operations such as "sine" and
"cosine" that are generic over ordinary numbers and rational
numbers.

|#

#| Answer - Rewrite from 2.73 |#
(load-ex "2.85")

; make constructors more type safe to speed debugging
(define (upgrade-makes)
  (put 'make 'integer (lambda (x)
    (if (integer? x)
        (attach-tag 'integer x)
        (error "make-integer" "not integers" x))))
  (put 'make 'rational
    (lambda (n d) 
      (if (and (integer? n) (integer? d))
          (attach-tag 'rational (let ([g (gcd n d)])
                                  (cons (/ n g) (/ d g))))
          (error "make-rational" "not integers" n d))))
  (put 'make 'real (lambda (x) 
    (if (real? x)
        (attach-tag 'real x)
        (error "make-real" "not real" x))))
  'done)

; from 2.83
(put-coercion 
  'real 
  'complex 
  (lambda (x) (make-complex-from-real-imag x (make-integer 0))))

(define (upgrade-rectangular-package)
  ;; internal
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z))
                      (square (imag-part z)))))
  (define (angle z)
    (atangent (imag-part z) (real-part z)))
  ;; interface
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-img 'rectangular
    (lambda (x y) (attach-tag 'rectangular (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (error "" "use poloar for mag-ang")))
  'done)

(define (upgrade-polar-package)
  ;; internal
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  ;; interface
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
    (lambda (x y) (error "" "use rectangular for real-imag")))
  (put 'make-from-mag-ang 'polar
    (lambda (r a) (attach-tag 'polar (make-from-mag-ang r a))))
  'done)

(define (upgrade-complex-package)
  ; copied from 2.77  then modified
  (define (tag z) (attach-tag 'complex z))
  (define (type-check func x y)
    (if (not (and (find (lambda (t) (eq? t (type-tag x))) '(integer rational real))
                  (find (lambda (t) (eq? t (type-tag y))) '(integer rational real))))
        (error func "must be integer, rational or real" x y)))
  (define (make-from-real-imag x y)
    (type-check "make-complex-from-real-imag" x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    (type-check "make-complex-from-mag-ang" r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'add '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2))))
  ; copied from 2.80 then modified
  (put '=zero? '(complex) 
    (lambda (x)
      (and (equ? (real-part x) (make-integer 0))
           (equ? (imag-part x) (make-integer 0)))))
  ;; copied from 2.85 then modified
  (put 'project '(complex) (lambda (x) 
    (real-part x)))
  'done)

#| Answer -- new operations |#
(define (upgrade-math)
  (put 'sine '(real) (lambda (x) 
    (make-real (sin x))))
  (put 'cosine '(real) (lambda (x) 
    (make-real (cos x))))
  (put 'atangent '(real real) (lambda (x y)
    (make-real (atan x y))))
  (put 'square '(real) (lambda (x)
    (make-real (* x x))))
  (put 'square-root '(real) (lambda (x) 
    (make-real (sqrt x))))
  'done)
(define (atangent x y) (apply-generic 'atangent x y))
(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))

(upgrade-makes)
(upgrade-rectangular-package)
(upgrade-polar-package)
(upgrade-complex-package)
(upgrade-math)

;; cosine
;; sine

#| Tests |#
(define a (make-complex-from-real-imag (make-integer 1) (make-integer 2)))
(define b (make-complex-from-real-imag (make-rational 1 1) (make-rational 2 1)))
(define c (make-complex-from-real-imag (make-real (/ 1 1)) (make-real (/ 2 1))))
(define abc (list a b c))

(define d (make-complex-from-mag-ang (make-integer 1) (make-integer 2)))
(define e (make-complex-from-mag-ang (make-rational 1 1) (make-rational 2 1)))
(define f (make-complex-from-mag-ang (make-real (/ 1 1)) (make-real (/ 2 1))))
(define def (list d e f))

(define z0 (make-complex-from-real-imag (make-integer 0) (make-integer 0)))
(define z1 (make-complex-from-mag-ang (make-integer 0) (make-integer 0)))

(define-test (map angle abc)
             '((rational 1246538638225297 . 1125899906842624)
               (rational 1246538638225297 . 1125899906842624)
               (rational 1246538638225297 . 1125899906842624)))
(define-test (map magnitude abc)
             '((rational 629397181890197 . 281474976710656)
               (rational 629397181890197 . 281474976710656)
               (rational 629397181890197 . 281474976710656)))
(define-test (map imag-part abc)
             '((integer . 2) (integer . 2) (integer . 2)))
(define-test (map real-part abc) 
             '((integer . 1) (integer . 1) (integer . 1)))


(define-test (map angle def)
             '((integer . 2) (integer . 2) (integer . 2)))
(define-test (map magnitude def)
             '((integer . 1) (integer . 1) (integer . 1)))
(define-test (map imag-part def)
             '((rational 4095111552621091 . 4503599627370496)
               (rational 4095111552621091 . 4503599627370496)
               (rational 4095111552621091 . 4503599627370496)))
(define-test (map real-part def)
             '((rational -7496634952020485 . 18014398509481984)
               (rational -7496634952020485 . 18014398509481984)
               (rational -7496634952020485 . 18014398509481984)))

(define-test (=zero? a) #f)
(define-test (=zero? d) #f)
(define-test (=zero? z0) #t)
(define-test (=zero? z1) #t)

(define-test (equ? a z0) #f)
(define-test (equ? d z0) #f)
(define-test (equ? a a) #t)
(define-test (equ? d d) #t)

(define-test (add a a)
             '(complex rectangular (integer . 2) integer . 4))

(define-test (sub a a) 
             '(integer . 0))

(define-test (mul a a)
             '(complex polar (rational 396140812571321726461764698809 . 79228162514264337593543950336)
                              rational 1246538638225297 . 562949953421312))

(define-test (div a a) 
             '(integer . 1))

#| Notes

; If apply-generic coerced args both higher and lower you
could technically delete all =zero? implementations except
for integer.

; helper to remember procedures
> (ex "2.85")
> (define (list-ops type)
  (map car
       (filter (lambda (x) 
                 (or (equal? type (cadr x))
                     (and (pair? (cadr x)) (find (lambda (x) (equal? x type)) (cadr x)))))
               op-table)))
> (list-ops 'rectangular)
(make-from-mag-ang make-from-real-imag 
  angle magnitude imag-part real-part)
> (list-ops 'polar)
(make-from-mag-ang make-from-real-imag 
  angle magnitude imag-part real-part)
> (list-ops 'complex)
(project =zero? equ? make-from-mag-ang make-from-real-imag
  div mul sub add angle magnitude imag-part real-part)

|#
