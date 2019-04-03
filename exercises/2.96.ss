#|

Exercise 2.96:

a. Implement the procedure "pseudoremainder-terms", which is
just like "remainder-terms" except that it multiplies the
dividend by the integerizing factor described above before
calling "div-terms". Modify "gcd-terms" to use
"pseudoremainder-terms", and verify that
"greatest-common-divisor" now produces an answer with
integer coefficients on the example in Exercise 2.95.

b. The GCD now has integer coefficients, but they are larger
than those of P_1. Modify "gcd-terms" so that it removes
common factors from the coefficients of the answer by
dividing all the coefficients by their (integer) greatest
common divisor.

|#

(load-ex "2.95")

(define (upgrade-gcd)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
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
  ; Copied from 2.94========================================
  (define (expand-order o)
    ; Test what char is next
    (define (is-end? ls)
      (null? ls))
    (define (is-expt? ls)
      (and (not (null? ls)) (eq? (car ls) #\^)))
    (define (is-minus? ls)
      (eq? (car ls) #\-))
    (define (is-num? ls)
      (and (not (null? ls)) (char-numeric? (car ls))))
    (define (is-sym? ls)
      (and (not (null? ls)) (char-alphabetic? (car ls)) (char-lower-case? (car ls))))
    (define (fail ls)
      (if (null? ls)
          (error "expand-order" "unexpected end of string")
          (error "expand-order" "invalid char" (car ls))))
    ; Iteration
    (define (take-expt sym nresult result ls)
      (cond [(and (or (is-end? ls) (is-sym? ls)) (not (null? nresult)))
             (iter (cons (cons sym (string->number (list->string (reverse nresult)))) result) ls)]
            [(or (and (is-minus? ls) (null? nresult)) (is-num? ls))
             (take-expt sym (cons (car ls) nresult) result (cdr ls))]
            [else
             (fail ls)]))
    (define (take-sym sym result ls)
      (cond [(or (is-end? ls) (is-sym? ls))
             (iter (cons (cons sym 1) result) ls)]
            [(is-expt? ls)
             (take-expt sym '() result (cdr ls))]
            [else
             (fail ls)]))
    (define (iter result ls)
      (cond [(is-end? ls)
             result]
            [(is-sym? ls)
             (take-sym (car ls) result (cdr ls))]
            [else
             (fail ls)]))
    (sort (lambda (a b) (char<? (car a) (car b)))
          (iter '() (string->list o))))
  ; ========================================================
  ; hack because tower of types system is a mess
  (define (de-rat result L)
    (if (empty-termlist? L)
        result
        (let* ([t (first-term L)]
               [o (order t)]
               [c (coeff t)]
               [cc (contents c)]
               [new-t (if (and (is-type? c 'rational) 
                               (is-type? (numer cc) 'integer)
                               (is-type? (denom cc) 'integer)
                               (= (gcd (contents (numer cc)) (contents (denom cc))) (contents (denom cc))))
                          (make-term o (make-integer (/ (contents (numer cc)) (contents (denom cc)))))
                          t)])
          (de-rat (adjoin-term new-t result) (rest-terms L)))))
  (define (de-int L)
    (define (my-gcd result l)
      (if (empty-termlist? l)
          (apply gcd result)
          (let* ([t (first-term l)]
                 [c (coeff t)])
            (if (is-type? c 'integer)
                (my-gcd (cons (contents c) result) (rest-terms l))
                1))))
    (define (simp x result l)
      (if (empty-termlist? l)
          result
          (let ([t (first-term l)])
            (simp x (adjoin-term (make-term (order t) (make-integer (/ (contents (coeff t)) x)))
                         result) (rest-terms l)))))
    (let ([x (my-gcd '() L)])
      (if (= x 1)
          L
          (simp x (make-sparse-termlist) L))))
  ; ========================================================
  (define (order-terms L1)
    (let ([eo (expand-order (order (first-term L1)))])
      (if (null? eo)
          0
          (cdar eo))))
  (define (interigizing-factor P Q)
    (let ([O_1 (order-terms P)]
          [O_2 (order-terms Q)]
          [c (coeff (first-term Q))])
      (expon c (make-integer (+ 1 O_1 (- O_2))))))
  (define (pseudoremainder-terms x y)
    (cadr (div-terms 
      (mul-term-by-all-terms (make-term "" (interigizing-factor x y)) x) y)))
  (define (gcd-terms a b)
    (define (gcd-iter a b)
      (if (empty-termlist? b)
          a
          (gcd-iter b (pseudoremainder-terms a b))))
    (de-int (de-rat (make-sparse-termlist) (gcd-iter a b))))
  (define (gcd-poly p1 p2)
    (make-poly (gcd-terms (term-list p1) (term-list p2))))

  ; --interface--
  (put 'greatest-common-divisor '(polynomial polynomial) (lambda (p1 p2) (gcd-poly p1 p2)))
  (put 'expon '(integer integer) (lambda (b e)
    (make-integer (expt b e))))
  (put 'expon '(rational integer) (lambda (b e)
    (if (or (not (is-type? (numer b) 'integer))
            (not (is-type? (denom b) 'integer)))
        (error "expon" "something" b e)
        (make-rational (expon (numer b) (make-integer e))
                       (expon (denom b) (make-integer e))))))
  (void))

(upgrade-gcd)
(define (expon b e) (apply-generic 'expon b e))

#| Tests |#

; GCD[11x^4 - 22x^3 + 18x^2 - 14x + 7, 13x^3 - 21x^2 + 3x + 5]
(define-test (greatest-common-divisor Q1 Q2)
             '(polynomial sparse-termlist (term "x^2" (integer . 1))
                                          (term "x" (integer . -2))
                                          (term "" (integer . 1))))

#| Notes

if P and Q are polynomials
O_1 = order of P
O_2 = order of Q
c = leading coefficient of Q

Multiply P by c^(1 + O_1 - O_2)

|#
