#|

Exercise 2.94: Using "div-terms", implement the procedure
"remainder-terms" and use this to define "gcd-terms" as
above. Now write a procedure "gcd-poly" that computes the
polynomial GCD of two polys. (The procedure should signal an
error if the two polys are not in the same variable.)
Install in the system a generic operation
"greatest-common-divisor" that reduces to "gcd-poly" for
polynomials and to ordinary "gcd" for ordinary numbers. As a
test, try

(define p1 (make-polynomial
            'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)

and check your result by hand.

|#

(load-ex "2.93")

#| Answer |#
(define (upgrade-greatest-common-divisor)
  ; Copied from 2.92========================================
  (define (term-list p)
    p)
  (define (make-poly term-list)
    (attach-tag 'polynomial term-list))
  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          [else
            (let ([t1 (first-term L1)]
                  [t2 (first-term L2)])
              (cond [(order-gt t1 t2)
                    (adjoin-term
                       t1 (add-terms (rest-terms L1) L2))]
                    [(order-lt t1 t2)
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2)))]
                    [else
                      (adjoin-term
                       (add t1 t2)
                       (add-terms (rest-terms L1) (rest-terms L2)))]))]))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term (mul t1 t2)
            (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (div-terms a b)
    (if (empty-termlist? a)
        (list a a)
        (let ([ha (first-term a)]
              [hb (first-term b)])
        (if (order-gt hb ha)
            (list (make-sparse-termlist) a)
            (let* ([_1 (div ha hb)]
                   [_2 (mul-term-by-all-terms _1 b)]
                   [_3 (add-terms a (negate-terms _2))]
                   [_4 (div-terms _3 b)]
                   [answer (car _4)]
                   [remainder (cadr _4)])
              (list (adjoin-term _1 answer) remainder))))))
  (define (negate-terms tl)
    (if (empty-termlist? tl)
        tl
        (adjoin-term (negate (first-term tl)) 
                     (negate-terms (rest-terms tl)))))
  ; ========================================================

  ; --operations--
  (define (remainder-terms x y)
    (cadr (div-terms x y)))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  (define (gcd-poly p1 p2)
    (make-poly (gcd-terms (term-list p1) (term-list p2))))

  ; --interface--
  (put 'greatest-common-divisor '(polynomial polynomial) (lambda (p1 p2) (gcd-poly p1 p2)))
  (void))

(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))
(upgrade-greatest-common-divisor)

#| Tests |#

; GCD[(x^4 - x^3 -2x^2 + 2x), (x^3 - x)] = (-x^2 + x)
(define-test (greatest-common-divisor 
               (make-polynomial (make-sparse-termlist
                 (make-term "x^4" (make-integer 1))
                 (make-term "x^3" (make-integer -1))
                 (make-term "x^2" (make-integer -2))
                 (make-term "x" (make-integer 2)))) 
               (make-polynomial (make-sparse-termlist
                 (make-term "x^3" (make-integer 1))
                 (make-term "x" (make-integer -1)))))
              '(polynomial sparse-termlist
                 (term "x^2" (rational (integer . -1) integer . 1))
                 (term "x" (rational (integer . 1) integer . 1))))

; by hand
; GCD[(x^4 - x^3 -2x^2 + 2x), (x^3 - x)]
; (x^4 - x^3 -2x^2 + 2x) / (x^3 - x) ==> (x-1) r (-x^2 + x)
; GCD[(x^3 - x), (-x^2 + x)]
; (x^3 - x) / (-x^2 + x) ==> (-x - 1) r 0
; GCD[0, (-x^2 + x)] ==> (-x^2 + x)
