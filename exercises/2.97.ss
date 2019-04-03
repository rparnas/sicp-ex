#|

Exercise 2.97:

a. Implement this algorithm as a procedure "reduce-terms"
that takes two term lists "n" and "d" as arguments and
returns a list "nn", "dd", which are "n" and "d" reduced to
lowest terms via the algorithm given above. Also write a
procedure "reduce-poly", analogous to "add-poly", that
checks to see if the two polys have the same variable. If
so, "reduce-poly" strips off the variable and passes the
problem to "reduce-terms", then reattaches the variable to
the two term lists supplied by "reduce-terms".

b. Define a procedure analogous to "reduce-terms" that does
what the original "make-rat" did for integers:

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

and define "reduce" as a generic operation that calls
"apply-generic" to dispatch to either "reduce-poly" (for
"polynomial" arguments) or "reduce-integers" (for
"scheme-number" arguments). You can now easily make the
rational-arithmetic package reduce fractions to lowest terms
by having "make-rat" call "reduce" before combining the
given numerator and denominator to form a rational number.
The system now handles rational expressions in either
integers or polynomials. To test your program, try the
example at the beginning of this extended exercise:

(define  p1 (make-polynomial 'x '((1 1) (0  1))))
(define  p2 (make-polynomial 'x '((3 1) (0 -1))))
(define  p3 (make-polynomial 'x '((1 1))))
(define  p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(add rf1 rf2)

See if you get the correct answer, correctly reduced to
lowest terms.

|#

(load-ex "2.96")
(no-regression)

#| Answer |#
(define (upgrade)
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
  ; Copied from 2.96========================================
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
    (gcd-iter a b))
  (define (gcd-poly p1 p2)
    (make-poly (gcd-terms (term-list p1) (term-list p2))))
; =========================================================
  (define (all-terms L)
    (define (iter result l)
      (if (empty-termlist? l)
          result
          (iter (append (list (first-term l)) result) (rest-terms l))))
    (iter '() L))
  (define (reduce-terms L1 L2)
    (define (de-rat L)
      (define (iter result l)
        (if (empty-termlist? l)
            result
            (let* ([t (first-term l)]
                   [o (order t)]
                   [c (coeff t)]
                   [new-t (cond [(and (is-type? c 'rational)
                                      (is-type? (denom (contents c)) 'integer)
                                      (equ? (denom (contents c)) (make-integer 1)))
                                 (make-term o (numer (contents c)))]
                                 [(and (is-type? c 'rational)
                                       (is-type? (denom (contents c)) 'integer)
                                       (equ? (denom (contents c)) (make-integer -1)))
                                 (make-term o (mul (make-integer -1)
                                                   (numer (contents c))))]
                                 [else t])])
              (iter (adjoin-term new-t result) (rest-terms l)))))
      (iter (make-sparse-termlist) L))
    (define (gcd-of-all-coeff L1 L2)
      (define (iter results terms)
        (if (null? terms)
            (apply gcd results)
            (let ([c (coeff (car terms))])
              (cond [(is-type? c 'integer)
                     (iter (cons (contents c) results) (cdr terms))]
                    [else
                     (error "reduce-terms" "non-integer coeff")]))))
      (iter '() (append (all-terms L1) (all-terms L2))))
    (let* ([Lgcd (gcd-terms L1 L2)]
           [O_1 (max (order-terms L1) (order-terms L2))]
           [O_2 (order-terms Lgcd)]
           [ifactor (make-term "" (expon (coeff (first-term Lgcd)) 
                                    (make-integer (+ 1 O_1 (- O_2)))))]
           [nn (de-rat (car (div-terms (mul-term-by-all-terms ifactor L1) Lgcd)))]
           [nd (de-rat (car (div-terms (mul-term-by-all-terms ifactor L2) Lgcd)))]
           [cgcd (make-sparse-termlist (make-term "" (make-integer (gcd-of-all-coeff nn nd))))])
      (list (de-rat (car (div-terms nn cgcd)))
            (de-rat (car (div-terms nd cgcd))))))

  (define (reduce-integers n d)
    (let* ([g (gcd n d)])
      (list (make-integer (/ n g)) (make-integer (/ d g)))))
  (define (reduce-poly p1 p2)
    (let ([answer (reduce-terms (term-list p1) (term-list p2))])
      (list (make-poly (car answer)) (make-poly (cadr answer)))))
  (define (reduce n d)
    (cond [(and (is-type? n 'integer)
                (is-type? d 'integer))
           (let ([ans (reduce-integers (contents n) (contents d))])
              (list (car ans) (cadr ans)))]
          [(and (is-type? n 'integer)
                (is-type? d 'polynomial))
           (reduce (make-polynomial (make-sparse-termlist (make-term "" n)))
                   d)]
          [(and (is-type? n 'polynomial)
                (is-type? d ' integer))
           (reduce n (make-polynomial (make-sparse-termlist (make-term "" d))))]
          [(and (is-type? n 'polynomial)
                (is-type? d 'polynomial))
           (let ([ans (reduce-poly (contents n) (contents d))])
              (list (car ans) (cadr ans)))]
          [else
            (error "reduce" "unsupported rational x")]))
  (define (make-rat n d)
    (if (not (find (lambda (x) (eq? x (type-tag n)))
                   '(integer polynomial)))
        (error "make-rational" "n type not supported" n))
    (if (not (find (lambda (x) (eq? x (type-tag d)))
                   '(integer polynomial)))
        (error "make-rational" "d type not supported" d))
    (let* ([ans (reduce n d)]
           [nn (car ans)]
           [dd (cadr ans)])
      (cond [(and (is-type? nn 'integer) (is-type? dd 'integer) (equ? nn dd))
             (attach-tag 'rational (cons (make-integer 1) (make-integer 1)))]
            [(and (is-type? nn 'integer) (equ? nn (make-integer 0)))
             (attach-tag 'rational (cons (make-integer 0) (make-integer 1)))]
            [(and (is-type? dd 'integer) (equ? dd (make-integer 1)))
             (attach-tag 'rational (cons nn (make-integer 1)))]
            [else (attach-tag 'rational (cons nn dd))])))
  (put 'make 'rational (lambda (n d) (make-rat n d)))
  (void))

(upgrade)

#| Tests |#
(define-test (make-rational (make-integer 5) (make-integer 10))
             '(rational (integer . 1) integer . 2))

(define pa (make-polynomial (make-sparse-termlist 
                 (make-term "x^2" (make-integer 75)))))
(define pb (make-polynomial (make-sparse-termlist 
                 (make-term "x" (make-integer 5)))))

(define-test (make-rational pa pb)               
             '(rational
               (polynomial sparse-termlist
                 (term "x" (integer . 15)))
                polynomial sparse-termlist
                 (term "" (integer . 1))))

; (x + 1)
(define p1 (make-polynomial (make-sparse-termlist 
             (make-term "x" (make-integer 1))
             (make-term "" (make-integer 1)))))

; (x^3 - 1)
(define p2 (make-polynomial (make-sparse-termlist 
             (make-term "x^3" (make-integer 1))
             (make-term "" (make-integer -1)))))

; x
(define p3 (make-polynomial (make-sparse-termlist
             (make-term "x" (make-integer 1)))))

; x^2 - 1
(define p4 (make-polynomial (make-sparse-termlist 
             (make-term "x^2" (make-integer 1))
             (make-term "" (make-integer -1)))))

; (x + 1) / (x^3 - 1)
(define rf1 (make-rational p1 p2))

; x / (x^2 -1)
(define rf2 (make-rational p3 p4))

; [(x + 1) / (x^3 - 1)] + [x / (x^2 -1)]
; [(-x^3 - 2x^2 - 3x -1) / (-x^4 - x^3 + x + 1)]
; The polynomial is as simplified as possible although
; the answer is negated from what I would actually like
(define-test (add rf1 rf2)
             '(rational
               (polynomial sparse-termlist 
                 (term "x^3" (integer . -1)) 
                 (term "x^2" (integer . -2))
                 (term "x" (integer . -3)) 
                 (term "" (integer . -1)))
                polynomial sparse-termlist 
                 (term "x^4" (integer . -1))
                 (term "x^3" (integer . -1)) 
                 (term "x" (integer . 1))
                 (term "" (integer . 1))))