#|

Exercise 2.88: Extend the polynomial system to include
subtraction of polynomials. (Hint: You may find it helpful
to define a generic negation operation.)

|#

(load-ex "2.87")

#| Answer |#

(define (install-287)
  ;; ============================================
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; ============================================
  ;; poly
  (define (negate-polynomial p)
    (define (negate-term t)
      (make-term (order t) (negate (coeff t))))
    (define (negate-termlist tlist)
      (if (empty-termlist? tlist)
          (the-empty-termlist)
          (let ([term (first-term tlist)])
            (adjoin-term (make-term (order term) (negate (coeff term)))
                         (negate-termlist (rest-terms tlist))))))
    (make-poly (variable p) (negate-termlist (term-list p))))
  ;; lower types
  (put 'negate '(real) (lambda (x)
    (make-real (- x))))
  (put 'negate '(complex) (lambda (x)
    (make-complex-from-real-imag (negate (real-part x)) 
                                 (negate (imag-part x)))))
  
  ;; poly
  (put 'negate '(polynomial) (lambda (p) 
    (attach-tag 'polynomial (negate-polynomial p))))
  (put 'sub '(polynomial polynomial) (lambda (a b)
    (add 
      (attach-tag 'polynomial a) 
      (attach-tag 'polynomial (negate-polynomial b)))))
  'done)

(define (install-negate)

  'done)
(define (negate x) (apply-generic 'negate x))

(install-287)

#| Tests |#

(define-test (negate (make-integer 1))
             '(integer . -1))
(define-test (negate (make-rational 1 2))
             '(rational -1 . 2))
(define-test (negate (make-real (/ 3 7)))
             '(rational -3 . 7))
(define-test (negate (make-complex-from-real-imag (make-integer 1) (make-rational 1 2)))
             '(complex rectangular (integer . -1) rational -1 . 2))

; x --> -x
(define a (make-polynomial 'x (list (list 1 (make-integer 1)))))
(define-test a
             '(polynomial x (1 (integer . 1))))
(define-test (negate a)
             '(polynomial x (1 (integer . -1))))

; 4x^2 + (3/2)x + (7 + 4i) --> -4x^2 + (3/2)x - (7 - 4i)
(define b (make-polynomial 'x (list 
  (list 2 (make-integer 4))
  (list 1 (make-rational 3 2))
  (list 0 (make-complex-from-real-imag (make-integer 7) (make-integer 4))))))
(define-test b
             '(polynomial x (2 (integer . 4))
                            (1 (rational 3 . 2))
                            (0 (complex rectangular (integer . 7) integer . 4))))
(define-test (negate b) 
             '(polynomial x (2 (integer . -4))
                            (1 (rational -3 . 2))
                            (0 (complex rectangular (integer . -7) integer . -4))))

;  2y^4 + (1 + 2i)y^3 + (4x^2 - (3/2)x + (7 + 4i))y^2 + 5
(define c (make-polynomial 'y (list 
  (list 4 (make-rational 2 1))
  (list 3 (make-complex-from-real-imag (make-integer 1) (make-integer 2)))
  (list 2 b)
  (list 0 (make-integer 5)))))
(define-test c
             '(polynomial y (4 (rational 2 . 1))
                            (3 (complex rectangular (integer . 1) integer . 2))
                            (2 (polynomial x (2 (integer . 4))
                                             (1 (rational 3 . 2))
                                             (0 (complex rectangular (integer . 7) integer . 4))))
                            (0 (integer . 5))))
(define-test (negate c)
             '(polynomial y (4 (integer . -2))
                            (3 (complex rectangular (integer . -1) integer . -2))
                            (2 (polynomial x (2 (integer . -4))
                                             (1 (rational -3 . 2))
                                             (0 (complex rectangular (integer . -7) integer . -4))))
                            (0 (integer . -5))))

(define-test (sub a a)
             '(polynomial x))
(define-test (sub b b)
             '(polynomial x))
(define-test (sub c c)
             '(polynomial y))

; [x] - [4x^2 + (3/2)x + (7 + 4i)] = -4x^2 - (1/2)x - 7 - 4i
(define-test (sub a b)
             '(polynomial x (2 (integer . -4))
                            (1 (rational -1 . 2))
                            (0 (complex rectangular (integer . -7) integer . -4))))

#| Notes

The "correct" way seems to be to support multiplying
polynomials against the other types, rather than having a
negate function.

|#