#|

Exercise 2.92: By imposing an ordering on variables, extend
the polynomial package so that addition and multiplication
of polynomials works for polynomials in different variables.
(This is not easy!)

|#

(load-ex "2.88")

#| Answer
   
   * rewrite of 2.90, 2.91
|#

(define (install-term-package)
  ; --order utilities--
  ; expand order in the format "x^2y^2" to "((#\x  . 2) (#\y . 2))"
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

  ; collapse and sort order in the format "((#\x . 2) (#\y . 2))" to "x^2y^2"
  (define (collapse-order expanded-o)
    (define (iter result ls)
      (if (null? ls)
          result
          (let* ([next (car ls)]
                 [sym (car next)]
                 [n (cdr next)])
            (cond [(= n 0) 
                   (iter result (cdr ls))]
                  [(= n 1) 
                   (iter (append (list sym) result) (cdr ls))]
                  [else
                   (iter (append (list sym #\^) (string->list (number->string n)) result) (cdr ls))]))))
    (list->string (iter '() (sort (lambda (a b) (char>? (car a) (car b))) ; backwards
                                  expanded-o))))

  ; multiply two expanded orders -- seems redundant with add-terms
  (define (mul-order o1 o2)
    (define (iter L1 L2)
      (cond [(null? L1) L2]
            [(null? L2) L1]
            [else
              (let* ([o1 (car L1)]
                     [o2 (car L2)]
                     [sym1 (car o1)]
                     [sym2 (car o2)]
                     [n1 (cdr o1)]
                     [n2 (cdr o2)])
                (cond [(char<? sym1 sym2) ; "char<?" is ">"
                        (append (list o1) (iter (cdr L1) L2))]
                      [(char>? sym1 sym2) ; "char>?" is "<"
                        (append (list o2) (iter L1 (cdr L2)))]
                      [else
                        (append (list (cons sym1 (+ n1 n2)))
                                (iter (cdr L1) (cdr L2)))]))]))
    (collapse-order (iter (expand-order o1) (expand-order o2))))
  (define (invert-order o1)
    (define (iter result ls)
      (if (null? ls)
          result
          (iter (append (list (cons (caar ls) (- (cdar ls)))) result) (cdr ls))))
    (collapse-order (iter '() (expand-order o1))))
  (define (div-order o1 o2)
    (mul-order o1 (invert-order o2)))

  ; --selectors--
  (define (order-term term)
    (car term))
  (define (coeff-term term)
    (cadr term))

  ; --constructors--
  (define (make-term o c)
    (if (not (find (lambda (x) (eq? x (type-tag c)))
                  '(integer rational real complex)))
        (error "make-term" "coeff type not supported" c))
    (attach-tag 'term (list o c)))

  ; --operations--
  (define (order-equ-term? t1 t2)
    (equal? (order-term t1) (order-term t2)))
  (define (order-lt-term t1 t2)
    (order-gt-term t2 t1))
  (define (order-gt-term t1 t2)
    (define (iter exorder1 exorder2)
      (cond [(null? exorder1) #f]
            [(null? exorder2) #t]
            [else (let ([osym1 (caar exorder1)]
                        [osym2 (caar exorder2)]
                        [on1 (cdar exorder1)]
                        [on2 (cdar exorder2)])
                    (cond [(char<? osym1 osym2) #t]
                          [(char<? osym2 osym1) #f]
                          [(> on1 on2) #t]
                          [(> on2 on1) #f]
                          [else (iter (cdr exorder1) (cdr exorder2))]))]))
    (iter (expand-order (order-term t1))
          (expand-order (order-term t2))))
  (define (add-term t1 t2)
    (if (not (equal? (order-term t1) (order-term t2)))
        (error "add-term" "can only add terms of the same order" t1 t2))
    (make-term (order-term t1)
               (add (coeff-term t1) (coeff-term t2))))
  (define (mul-term t1 t2)
    (make-term (mul-order (order-term t1) (order-term t2))
               (mul (coeff-term t1) (coeff-term t2))))
  (define (div-term t1 t2)
    (make-term (div-order (order-term t1) (order-term t2))
               (div (coeff-term t1) (coeff-term t2))))
  (define (negate-term t1)
    (make-term (order-term t1)
               (negate (coeff-term t1))))

  ; --interface--
  (put 'order '(term) (lambda (t) (order-term t)))
  (put 'coeff '(term) (lambda (t) (coeff-term t)))
  (put 'make 'term (lambda (o c) (make-term o c)))
  (put 'order-equ? '(term term) (lambda (t1 t2) (order-equ-term? t1 t2)))
  (put 'order-gt '(term term) (lambda (t1 t2) (order-gt-term t1 t2)))
  (put 'order-lt '(term term) (lambda (t1 t2) (order-lt-term t1 t2)))
  (put 'add '(term term) (lambda (t1 t2) (add-term t1 t2)))
  (put 'mul '(term term) (lambda (t1 t2) (mul-term t1 t2)))
  (put 'div '(term term) (lambda (t1 t2) (div-term t1 t2)))
  (put 'negate '(term) (lambda (t1) (negate-term t1)))
  (void))

(define (install-sparse-termlist-package)
  ; --selectors--
  (define (first-term-stl stl)
    (car stl))
  (define (rest-terms-stl stl)
    (attach-tag 'sparse-termlist (cdr stl)))
  (define (empty-termlist-stl? stl)
    (null? stl))

  ; --constructors--
  (define (make-sparse-termlist terms)
    (attach-tag 'sparse-termlist
                (sort order-gt terms)))

  ; --operators--
  (define (adjoin-term term stl)
    (if (=zero? (coeff (attach-tag 'term term)))
        (attach-tag 'sparse-termlist stl)
        (attach-tag 'sparse-termlist (cons (attach-tag 'term term) stl))))

  ; --interface--
  (put 'first-term '(sparse-termlist) (lambda (stl) (first-term-stl stl)))
  (put 'rest-terms '(sparse-termlist) (lambda (stl) (rest-terms-stl stl)))
  (put 'empty-termlist? '(sparse-termlist) (lambda (stl) (empty-termlist-stl? stl)))
  (put 'make 'sparse-termlist (lambda (terms) (make-sparse-termlist terms)))
  (put 'adjoin-term '(term sparse-termlist) (lambda (term stl) (adjoin-term term stl)))
  (void))

(define (install-polynomial-package)
  ; --selectors--
  (define (term-list p)
    p)

  ; --constructors--
  (define (make-poly term-list)
    (attach-tag 'polynomial term-list))

  ; --operations--
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
  (define (add-poly p1 p2)
    (make-poly (add-terms (term-list p1) (term-list p2))))
  (define (mul-poly p1 p2)
    (make-poly (mul-terms (term-list p1) (term-list p2))))
  (define (div-poly p1 p2)
    (let* ([result (div-terms (term-list p1) (term-list p2))]
           [answer (car result)]
           [remainder (cadr result)])
      (list (make-polynomial answer)
            (make-polynomial remainder))))
  (define (negate-poly p1)
    (make-poly (negate-terms (term-list p1))))
  (define (=zero-poly? p)
    (define (iter tl)
      (cond [(empty-termlist? tl) #t]
            [(not (=zero? (coeff (first-term tl)))) #f]
            [else (iter (rest-terms tl))]))
    (iter (term-list p)))

  ; --interface--
  (put 'make 'polynomial (lambda (tl) (make-poly tl)))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (add-poly p1 p2)))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (mul-poly p1 p2)))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (div-poly p1 p2)))
  (put 'negate '(polynomial) (lambda (p1) (negate-poly p1)))
  (put '=zero? '(polynomial) (lambda (p1) (=zero-poly? p1)))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2)
    (add (attach-tag 'polynomial p1)
         (negate (attach-tag 'polynomial p2)))))
  (void))

; --interface--
(define (make-term order coeff) ((get 'make 'term) order coeff))
(define (order x) (apply-generic 'order x))
(define (coeff x) (apply-generic 'coeff x))
(define (order-equ? x y) (apply-generic 'order-equ? x y))
(define (order-lt x y) (apply-generic 'order-lt x y))
(define (order-gt x y) (apply-generic 'order-gt x y))
(define (first-term tl) (apply-generic 'first-term tl))
(define (rest-terms tl) (apply-generic 'rest-terms tl))
(define (empty-termlist? tl) (apply-generic 'empty-termlist? tl))
(define (make-sparse-termlist . terms) ((get 'make 'sparse-termlist) terms))
(define (adjoin-term term tl) (apply-generic 'adjoin-term term tl))
(define (make-polynomial termlist) ((get 'make 'polynomial) termlist))

; Installation
(install-term-package)
(install-sparse-termlist-package)
(install-polynomial-package)

#| Tests - term

> (make-term "x" (make-integer 2))
(term "x" (integer . 2))
> (make-term "x" (make-integer 4))
(term "x" (integer . 4))
> (add (make-term "x" (make-integer 2)) (make-term "x" (make-integer 2)))
(term "x" (integer . 4))
> (mul (make-term "x" (make-integer 3)) (make-term "x" (make-integer 2)))
(term "x^2" (integer . 6))
> (mul (make-term "x" (make-integer 3)) (make-term "y" (make-integer 2)))
(term "xy" (integer . 6))
> (mul (make-term "x" (make-integer 3)) (make-term "x^-1" (make-integer 2)))
(term "" (integer . 6))

x^3y^2 > x^3y > x^3 > x^2y^9 > x^2y > x^2 > xy > x > x^-1 > y > constant
> (order-gt (make-term "x^3y^2" (make-integer 1))
            (make-term "x^3y" (make-integer 1)))
#t
> (order-gt (make-term "x^3y" (make-integer 1))
            (make-term "x^3" (make-integer 1)))
#t
> (order-gt (make-term "x^3" (make-integer 1))
            (make-term "x^2y^9" (make-integer 1)))
#t
> (order-gt (make-term "x^2y^9" (make-integer 1))
            (make-term "x^2y" (make-integer 1)))
#t
> (order-gt (make-term "x^2y" (make-integer 1))
            (make-term "x^2" (make-integer 1)))
#t
> (order-gt (make-term "x^2" (make-integer 1))
            (make-term "xy" (make-integer 1)))
#t
> (order-gt (make-term "xy" (make-integer 1))
            (make-term "x" (make-integer 1)))
#t
> (order-gt (make-term "x" (make-integer 1))
            (make-term "y" (make-integer 1)))
#t
> (order-gt (make-term "x" (make-integer 1))
            (make-term "x^-1" (make-integer 1)))
#t
> (order-gt (make-term "x^-1" (make-integer 1))
            (make-term "y" (make-integer 1))) 
#t
> (order-gt (make-term "y" (make-integer 1))
            (make-term "" (make-integer 1)))
#t                             

|#

#| Tests - sparse termlist

> (make-sparse-termlist)
(sparse-termlist)
> (make-sparse-termlist 
    (make-term "x" (make-integer 2)))
(sparse-termlist (term "x" (integer . 2)))
> (make-sparse-termlist 
    (make-term "x" (make-integer 1))
    (make-term "y" (make-integer 2)))
(sparse-termlist
  (term "y" (integer . 2))
  (term "x" (integer . 1)))
> (make-sparse-termlist
      (make-term "y" (make-integer 2))
      (make-term "x" (make-integer 1)))
(sparse-termlist
  (term "x" (integer . 1))
  (term "y" (integer . 2)))

|#

#| Test -- polynomial

> (make-polynomial (make-sparse-termlist
           (make-term "x" (make-integer 1))
           (make-term "" (make-integer 1))))
(polynomial
  sparse-termlist
  (term "x" (integer . 1))
  (term "" (integer . 1)))

; x + 1
> (add (make-polynomial (make-sparse-termlist (make-term "x" (make-integer 1))))
         (make-polynomial (make-sparse-termlist (make-term "" (make-integer 1)))))
(polynomial
  sparse-termlist
  (term "x" (integer . 1))
  (term "" (integer . 1)))

; (x + 1) + (y + 1)
> (add (make-polynomial (make-sparse-termlist
         (make-term "x" (make-integer 1))
         (make-term "" (make-integer 1))))
       (make-polynomial (make-sparse-termlist
         (make-term "y" (make-integer 1))
         (make-term "" (make-integer 1)))))
(polynomial
  sparse-termlist
  (term "x" (integer . 1))
  (term "y" (integer . 1))
  (term "" (integer . 2)))

; (x) * (x)
> (mul (make-polynomial (make-sparse-termlist (make-term "x" (make-integer 1))))
       (make-polynomial (make-sparse-termlist (make-term "x" (make-integer 1)))))
(polynomial sparse-termlist (term "x^2" (integer . 1)))

; (x - 2) * (x - 4)
> (mul (make-polynomial (make-sparse-termlist (make-term "x" (make-integer 1)) (make-term "" (make-integer -2))))
       (make-polynomial (make-sparse-termlist (make-term "x" (make-integer 1)) (make-term "" (make-integer -4)))))
(polynomial
  sparse-termlist
  (term "x^2" (integer . 1))
  (term "x" (integer . -6))
  (term "" (integer . 8)))

(x^2 -5y + 6)
> (negate (make-polynomial (make-sparse-termlist (make-term "x^2" (make-integer 1))
                                                 (make-term "y" (make-integer -5))
                                                 (make-term "" (make-integer 6)))))
(polynomial
  sparse-termlist
  (term "x^2" (integer . -1))
  (term "y" (integer . -5))
  (term "" (integer . 6)))

(x^2) - (x^2)
> (define a (make-polynomial (make-sparse-termlist (make-term "x^2" (make-integer 1)))))
> (sub a a)
(polynomial sparse-termlist)
> (=zero? (sub a a))
#t

; x / y
> (div (make-polynomial (make-sparse-termlist (make-term "x" (make-integer 1))))
       (make-polynomial (make-sparse-termlist (make-term "y" (make-integer 1)))))
((polynomial sparse-termlist (term "xy^-1" (integer . 1)))
 (polynomial sparse-termlist))

; (x-1) / (x^2 - 1) = 0 r (x -1)
> (div (make-polynomial (make-sparse-termlist
         (make-term "x" (make-integer 1))
        (make-term "" (make-integer -1))))
       (make-polynomial (make-sparse-termlist
         (make-term "x^2" (make-integer 1))
         (make-term "" (make-integer -1)))))
((polynomial sparse-termlist)
 (polynomial sparse-termlist
   (term "x" (integer . 1))
   (term "" (integer . -1))))

; (x^3 - 1) / (x^2 -1) = (x) r (x-1)
> (div (make-polynomial (make-sparse-termlist
         (make-term "x^3" (make-integer 1))
         (make-term "" (make-integer -1))))
       (make-polynomial (make-sparse-termlist
         (make-term "x^2" (make-integer 1))
         (make-term "" (make-integer -1)))))
((polynomial sparse-termlist 
   (term "x" (integer . 1)))
 (polynomial sparse-termlist 
   (term "x" (integer . 1))
   (term "" (integer . -1))))

; (x^5 - 1) / (x^2 - 1) = (x^3 + x) r (x-1)
> (div (make-polynomial (make-sparse-termlist
         (make-term "x^5" (make-integer 1))
         (make-term "" (make-integer -1))))
       (make-polynomial (make-sparse-termlist
         (make-term "x^2" (make-integer 1))
         (make-term "" (make-integer -1)))))
((polynomial x sparse-termlist
  (term "x^3" (integer . 1)) 
  (term "x" (integer . 1)))
 (polynomial x sparse-termlist
  (term "x" (integer . 1))
  (term "" (integer . -1))))

|#

#| Notes

Did not fully test polynomial division of mixed symbols.

|#
