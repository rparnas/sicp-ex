#|

Exercise 2.75: Implement the constructor "make-from-mag-ang"
in message-passing style. This procedure should be analogous
to the "make-from-real-imag" procedure given above.

|#

#| Code from book |#
(define (apply-generic op arg) (arg op))

#| Answer |#
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* mag (cos ang))]
          [(eq? op 'imag-part) (* mag (sin ang))]
          [(eq? op 'magnitude) mag]
          [(eq? op 'angle) ang]
          [else (error "Unknown op" op)]))
  dispatch)

#| Tests |#
(define (real-part x) (apply-generic 'real-part x))
(define (imag-part x) (apply-generic 'imag-part x))
(define (magnitude x) (apply-generic 'magnitude x))
(define (angle x) (apply-generic 'angle x))
(define x (make-from-mag-ang 5 7))

#|

> (real-part x)
3.769511271716523
> (imag-part x)
3.2849329935939453
|#

(define-test (magnitude x) 5)
(define-test (angle x) 7)
