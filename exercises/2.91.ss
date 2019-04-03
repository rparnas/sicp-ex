#|

Exercise 2.91: A univariate polynomial can be divided by
another one to produce a polynomial quotient and a
polynomial remainder. For example,

x^5 - 1
------- = x^3 + x, remainder x - 1
x^2 - 1

Division can be performed via long division. That is, divide
the highest-order term of the dividend by the highest-order
term of the divisor. The result is the first term of the
quotient. Next, multiply the result by the divisor, subtract
that from the dividend, and produce the rest of the answer
by recursively dividing the difference by the divisor. Stop
when the order of the divisor exceeds the order of the
dividend and declare the dividend to be the remainder. Also,
if the dividend ever becomes zero, return zero as both
quotient and remainder.

We can design a "div-poly" procedure on the model of
"add-poly" and "mul-poly". The procedure checks to see if
the two polys have the same variable. If so, "div-poly"
strips off the variable and passes the problem to
"div-terms", which performs the division operation on term
lists. "div-poly" finally reattaches the variable to the
result supplied by "div-terms". It is convenient to design
"div-terms" to compute both the quotient and the remainder
of a division. "div-terms" can take two term lists as
arguments and return a list of the quotient term list and
the remainder term list.

Complete the following definition of "div-terms" by filling
in the missing expressions. Use this to implement
"div-poly", which takes two polys as arguments and returns a
list of the quotient and remainder polys.

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                       <  compute rest of result recursively   >  ))
                  <  form complete result   >  ))))))

|#

(load-ex "2.90")

 (define (install-poly-div)
  ; ======================================
  (define (term-list p)
    (cdr p))
  (define (variable p)
    (car p))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                      t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                      t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (negate-terms tl)
    (if (empty-termlist? tl)
        tl
        (let ([term (first-term tl)])
          (adjoin-term (make-term (order term) (negate (coeff term)))
                       (negate-terms (rest-terms tl))))))
  ; ======================================
  ;       a / b
  ; (1) | ha / hb
  ; (2) | (1) * b
  ; (3) | a - (2)
  ; (4) | (3) / b
  ; (*) Stop if divisor order exceeds dividend order
  ; (*) Zero dividend implies 0 remainder 0
  (define (div-terms a b)
    (if (empty-termlist? a)
        (list a a)
        (let ([ha (first-term a)]
              [hb (first-term b)])
        (if (> (order hb) (order ha))
            (list (make-sparse-termlist) a)
            (let* ([_1 (make-term (- (order ha) (order hb))
                                      (div (coeff ha) (coeff hb)))]
                   [_2 (mul-term-by-all-terms _1 b)]
                   [_3 (add-terms a (negate-terms _2))]
                   [_4 (div-terms _3 b)]
                   [answer (car _4)]
                   [remainder (cadr _4)])
              (list (adjoin-term _1 answer) remainder))))))

  (put 'div '(polynomial polynomial) (lambda (p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let* ([result (div-terms (term-list p1) (term-list p2))]
               [answer (car result)]
               [remainder (cadr result)])
        (list (make-polynomial (variable p1) answer)
              (make-polynomial (variable p1) remainder))))))
                         
  'done)

 (install-poly-div)

#| Tests
; (x^5 - 1) / (x^2 - 1) = (x^3 + x) r (x-1)
> (div (make-polynomial 'x (make-sparse-termlist 
         (make-term 5 (make-integer 1))
         (make-term 0 (make-integer -1))))
       (make-polynomial 'x (make-sparse-termlist
         (make-term 2 (make-integer 1))
         (make-term 0 (make-integer -1)))))
((polynomial x sparse-termlist
  (term 3 (integer . 1)) 
  (term 1 (integer . 1)))
 (polynomial x sparse-termlist
  (term 1 (integer . 1))
  (term 0 (integer . -1))))

; (-x^2 + x) / (x^3 - x) = 0 r (x^2 + x)
> (div (make-polynomial 'x (make-sparse-termlist 
         (make-term 2 (make-integer -1))
         (make-term 1 (make-integer 1))))
       (make-polynomial 'x (make-sparse-termlist
         (make-term 3 (make-integer 1))
         (make-term 1 (make-integer -1)))))
((polynomial x sparse-termlist)
 (polynomial x sparse-termlist
   (term 2 (integer . -1))
   (term 1 (integer . 1))))

; (-x^3 - x^2 + 2x) / (x^3 - x) = -1 r (x^2 + x)
; (1) | -x^3 / x^3 = -1
; (2) | -1 * (x^3 - x) = -x^3 + x
; (3) | (-x^3 - x^2 + 2x) - (-x^3 + x) = x^2 + x
; (4) | (x^2 + x) / (x^3 - x) = 0 r (x^2 + x)
> (div (make-polynomial 'x (make-sparse-termlist
         (make-term 3 (make-integer -1))
         (make-term 2 (make-integer -1))
         (make-term 1 (make-integer 2))))
       (make-polynomial 'x (make-sparse-termlist
         (make-term 3 (make-integer 1))
         (make-term 1 (make-integer -1)))))
((polynomial x sparse-termlist 
   (term 0 (integer . -1)))
 (polynomial x sparse-termlist 
   (term 2 (integer . -1))
   (term 1 (integer . 1))))

; (x^4 - x^3 -2x^2 + 2x) / (x^3 - x) = (x-1) r (-x^2 + x)
; (1) |  x^4 / x^3 = x
; (2) | x(x^3 - x) = x^4 - x^2
; (3) | (x^4 - x^3 - 2x^2 + 2x) - (x^4 - x^2) = -x^3 - x^2 + 2x
; (4) | (-x^3 - x^2 + 2x) / (x^3 - x) = -1 r (x^2 + x)
> (div (make-polynomial 'x (make-sparse-termlist
         (make-term 4 (make-integer 1))
         (make-term 3 (make-integer -1))
         (make-term 2 (make-integer -2))
         (make-term 1 (make-integer 2)))) 
       (make-polynomial 'x (make-sparse-termlist
         (make-term 3 (make-integer 1))
         (make-term 1 (make-integer -1)))))
|#
