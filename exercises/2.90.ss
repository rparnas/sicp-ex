#|

Exercise 2.90: Suppose we want to have a polynomial system
that is efficient for both sparse and dense polynomials. One
way to do this is to allow both kinds of term-list
representations in our system. The situation is analogous to
the complex-number example of Section 2.4, where we allowed
both rectangular and polar representations. To do this we
must distinguish different types of term lists and make the
operations on term lists generic. Redesign the polynomial
system to implement this generalization. This is a major
effort, not a local change.

|#

(load-ex "2.86")

#| Answer
   
   * rewrite of 2.87, 2.88, 2.89 
   * underscore indicates operation on unpacked type
|#

(define (install-polynomial-package)
  ; internal - term selectors
  (define (o term)
    (car term))
  (define (c term)
    (cadr term))

  ; internal dense-termlist
  (define (adjoin-term-d term dtl)
    (define (zeros n)
      (define (iter result n)
        (if (= 0 n)
            result
            (iter (cons (make-integer 0) result) (- n 1))))
      (iter '() n))
    (if (=zero? (c term))
        (attach-tag 'dense-termlist dtl)
        (let ([ft-order (if (empty-termlist-d? dtl)
                            -1
                            (order (first-term-d dtl)))])
          (if (< (o term) ft-order)
              (error "adjoin-term" 
                     "must build up list in order"
                     term term-list))
          (attach-tag 'dense-termlist 
            (append (list (c term)) 
                    (zeros (- (o term) ft-order 1)) 
                    dtl)))))
  (define (empty-termlist-d? dtl)
    (null? dtl))
  (define (first-term-d dtl)
    (make-term (- (length dtl) 1) (car dtl)))
  (define (rest-terms-d dtl)
    (attach-tag 'dense-termlist 
      (or (memp (lambda (x) (not (eqv? x (make-integer 0)))) (cdr dtl))
          '())))
  (define (the-empty-termlist-d)
    (attach-tag 'dense-termlist '()))

  ; internal - sparse-termlist
  (define (adjoin-term-s term stl)
    (if (=zero? (c term))
        (attach-tag 'sparse-termlist stl)
        (attach-tag 'sparse-termlist 
                     (cons (attach-tag 'term term) stl))))
  (define (empty-termlist-s? stl)
    (null? stl))
  (define (first-term-s stl)
    (car stl))
  (define (rest-terms-s stl)
    (attach-tag 'sparse-termlist (cdr stl)))
  (define (the-empty-termlist-s)
    (attach-tag 'sparse-termlist '()))

  ; internal - poly
  (define (term-list p)
    (cdr p))
  (define (variable p)
    (car p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x)
    (symbol? x))
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
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
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

  ; term
  (put 'make 'term (lambda (order coeff)
    (if (not (integer? order))
        (error "make-term" "order must be integer" order))
    (if (not (find (lambda (t) (eq? t (type-tag coeff))) 
                  '(integer rational real complex polynomial)))
        (error "make-term" "coeff type not supported" coeff))
    (attach-tag 'term (list order coeff))))
  (put 'order '(term) o)
  (put 'coeff '(term) c)

  ; dense-termlist
  (put 'adjoin-term '(term dense-termlist) adjoin-term-d)
  (put 'empty-termlist? '(dense-termlist) empty-termlist-d?)
  (put 'first-term '(dense-termlist) first-term-d)
  (put 'rest-terms '(dense-termlist) rest-terms-d)
  (put 'the-empty-termlist 'dense-termlist the-empty-termlist-d)

  ; sparse-termlist
  (put 'adjoin-term '(term sparse-termlist) adjoin-term-s)
  (put 'empty-termlist? '(sparse-termlist) empty-termlist-s?)
  (put 'first-term '(sparse-termlist) first-term-s)
  (put 'rest-terms '(sparse-termlist) rest-terms-s)
  (put 'the-empty-termlist 'sparse-termlist the-empty-termlist-s)

  ; poly
  (put 'make 'polynomial (lambda (variable term-list)
    (attach-tag 'polynomial (cons variable term-list))))
  (put 'add '(polynomial polynomial) (lambda (p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-polynomial (variable p1)
                         (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2)))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-polynomial (variable p1)
                         (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in the same var: MUL-POLY" (list p1 p2)))))
  (put 'negate '(polynomial) (lambda (p)
    (make-polynomial (variable p) (negate-terms (term-list p)))))
  (put '=zero? '(polynomial) (lambda (p)
    (define (iter tl)
      (cond [(empty-termlist? tl) #t]
            [(not (=zero? (coeff (first-term tl)))) #f]
            [else (iter (rest-terms tl))]))
    (iter (term-list p))))
  (put 'sub '(polynomial polynomial) (lambda (a b)
    (add 
      (attach-tag 'polynomial a) 
      (negate (attach-tag 'polynomial b)))))
  (set! adjoin-term-d (tracize adjoin-term-d))
  'done)

; terms
(define (make-term order coeff)
  ((get 'make 'term) order coeff))
(define (order term)
  (apply-generic 'order term))
(define (coeff term)
  (apply-generic 'coeff term))

; termlists
(define (make-termlist type . terms)
  (define (iter term-list terms)
    (if (null? terms)
        term-list
        (iter (adjoin-term (car terms) term-list) (cdr terms))))
  (iter ((get 'the-empty-termlist type)) (reverse terms)))
(define (make-dense-termlist . terms)
  (apply make-termlist (append (list 'dense-termlist) terms)))
(define (make-sparse-termlist . terms)
  (apply make-termlist (append (list 'sparse-termlist) terms)))
(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list)
  (apply-generic 'empty-termlist? term-list))
(define (adjoin-term term term-list)
  (apply-generic 'adjoin-term term term-list))
(define (negate x) (apply-generic 'negate x))

; poly
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; lower types
(define (upgrade-lower-types)
  (put 'negate '(real) (lambda (x)
    (make-real (- x))))
  (put 'negate '(complex) (lambda (x)
    (make-complex-from-real-imag (negate (real-part x)) 
                                 (negate (imag-part x)))))
  'done)

(install-polynomial-package)
(upgrade-lower-types)

#| Tests from 2.86

> (make-term 1 (make-integer 2))
(term 1 (integer . 2))

> (make-sparse-termlist 
    (make-term 2 (make-integer 2)) 
    (make-term 0 (make-integer 2)))
(sparse-termlist
  (term 2 (integer . 2))
  (term 0 (integer . 2)))
> (make-dense-termlist 
   (make-term 2 (make-integer 2)) 
   (make-term 0 (make-integer 2)))
(dense-termlist 
  (integer . 2) 
  (integer . 0)
  (integer . 2))

> (=zero? (make-polynomial 'x (make-dense-termlist)))
#t
> (=zero? (make-polynomial 'x (make-sparse-termlist)))
#t

> (=zero? (make-polynomial 'x (make-dense-termlist 
            (make-term 5 (make-integer 0)))))
#t
> (=zero? (make-polynomial 'x (make-sparse-termlist 
            (make-term 5 (make-integer 0)))))
#t

> (=zero? (make-polynomial 'x (make-dense-termlist
            (make-term 5 (make-polynomial 'y (make-dense-termlist (make-term 5 (make-integer 0))))))))
#t

> (=zero? (make-polynomial 'x (make-sparse-termlist
            (make-term 5 (make-polynomial 'y (make-sparse-termlist (make-term 5 (make-integer 0))))))))
#t

> (define ad (make-polynomial 'x (make-dense-termlist (make-term 5 (make-integer 1)))))
> (define as (make-polynomial 'x (make-sparse-termlist (make-term 5 (make-integer 1)))))

> (=zero? ad)
#f
> (=zero? as)
#f

> (add ad ad)
(polynomial x dense-termlist (integer . 2) (integer . 0)
  (integer . 0) (integer . 0) (integer . 0) (integer . 0))
> (add as as)
(polynomial x sparse-termlist 
  (term 5 (integer . 2)))
> (add ad as)
(polynomial x dense-termlist (integer . 2) (integer . 0)
  (integer . 0) (integer . 0) (integer . 0) (integer . 0))
> (add as ad)
(polynomial x dense-termlist (integer . 2) (integer . 0)
  (integer . 0) (integer . 0) (integer . 0) (integer . 0))

> (mul ad as)
(polynomial x sparse-termlist (term 10 (integer . 1)))

> (define bd (make-polynomial 'x (make-dense-termlist 
    (make-term 5 (make-polynomial 'y (make-dense-termlist
                   (make-term 5 (make-integer 1))))))))
> (define bs (make-polynomial 'x (make-sparse-termlist 
    (make-term 5 (make-polynomial 'y (make-sparse-termlist
                   (make-term 5 (make-integer 1))))))))

> (=zero? bs)
#f
> (=zero? bd)
#f

> (add bs bs)
(polynomial x sparse-termlist
  (term 5 (polynomial y sparse-termlist 
            (term 5 (integer . 2)))))
> (add bd bd)
(polynomial x dense-termlist
  (polynomial y dense-termlist 
    (integer . 2) 
    (integer . 0)
    (integer . 0) 
    (integer . 0) 
    (integer . 0) 
    (integer . 0))
  (integer . 0) 
  (integer . 0) 
  (integer . 0) 
  (integer . 0)
  (integer . 0))

|#

#| Tests from 2.87

; x --> -x
> (define ad (make-polynomial 'x (make-dense-termlist (make-term 1 (make-integer 1)))))
> (define as (make-polynomial 'x (make-sparse-termlist (make-term 1 (make-integer 1)))))
> ad
(polynomial x dense-termlist (integer . 1) (integer . 0))
> as
(polynomial x sparse-termlist (term 1 (integer . 1)))
> (negate ad)
(polynomial x dense-termlist (integer . -1) (integer . 0))
> (negate as)
(polynomial x sparse-termlist (term 1 (integer . -1)))

; 4x^2 + (3/2)x + (7 + 4i) --> -4x^2 + (3/2)x - (7 - 4i)
> (define bd (make-polynomial 'x (make-dense-termlist
    (make-term 2 (make-integer 4))
    (make-term 1 (make-rational 3 2))
    (make-term 0 (make-complex-from-real-imag (make-integer 7) (make-integer 4))))))
> bd
(polynomial x dense-termlist 
  (integer . 4) 
  (rational 3 . 2)
  (complex rectangular (integer . 7) integer . 4))
> (define bs (make-polynomial 'x (make-sparse-termlist
      (make-term 2 (make-integer 4))
      (make-term 1 (make-rational 3 2))
      (make-term 0 (make-complex-from-real-imag (make-integer 7) (make-integer 4))))))
> (negate bs)
(polynomial x sparse-termlist 
  (term 2 (integer . -4))
  (term 1 (rational -3 . 2))
  (term 0 (complex rectangular (integer . -7) integer . -4)))
> bs
(polynomial x sparse-termlist 
  (term 2 (integer . 4))
  (term 1 (rational 3 . 2))
  (term 0 (complex rectangular (integer . 7) integer . 4)))
> (negate bd)
(polynomial x dense-termlist 
  (integer . -4)
  (rational -3 . 2)
  (complex rectangular (integer . -7) integer . -4))

;  2y^4 + (1 + 2i)y^3 + (4x^2 - (3/2)x + (7 + 4i))y^2 + 5
> (define cd (make-polynomial 'y (make-dense-termlist
    (make-term 4 (make-rational 2 1))
    (make-term 3 (make-complex-from-real-imag (make-integer 1) (make-integer 2)))
    (make-term 2 bd)
    (make-term 0 (make-integer 5)))))
> cd
(polynomial y dense-termlist (rational 2 . 1)
  (complex rectangular (integer . 1) integer . 2)
  (polynomial x dense-termlist 
    (integer . 4) 
    (rational 3 . 2)
    (complex rectangular (integer . 7) integer . 4))
  (integer . 0) 
  (integer . 5))
> (negate cd)
(polynomial y dense-termlist 
  (integer . -2)
  (complex rectangular (integer . -1) integer . -2)
  (polynomial x dense-termlist 
    (integer . -4)
    (rational -3 . 2)
    (complex rectangular (integer . -7) integer . -4))
  (integer . 0) 
  (integer . -5))

> (define cs (make-polynomial 'y (make-sparse-termlist
    (make-term 4 (make-rational 2 1))
    (make-term 3 (make-complex-from-real-imag (make-integer 1) (make-integer 2)))
    (make-term 2 bs)
    (make-term 0 (make-integer 5)))))
> cs
(polynomial y sparse-termlist 
  (term 4 (rational 2 . 1))
  (term 3 (complex rectangular (integer . 1) integer . 2))
  (term 2 (polynomial x sparse-termlist 
    (term 2 (integer . 4))
    (term 1 (rational 3 . 2))
    (term 0 (complex rectangular (integer . 7) integer . 4))))
  (term 0 (integer . 5)))
> (negate cs)
(polynomial y sparse-termlist (term 4 (integer . -2))
  (term 3 (complex rectangular (integer . -1) integer . -2))
  (term 2 (polynomial x sparse-termlist 
    (term 2 (integer . -4))
    (term 1 (rational -3 . 2))
    (term 0 (complex rectangular (integer . -7) integer . -4))))
  (term 0 (integer . -5)))

> (sub ad as)
(polynomial x dense-termlist (integer . 0))
> (sub bd bs)
(polynomial x sparse-termlist)
> (sub cd cs)
(polynomial y sparse-termlist)

; [x] - [4x^2 + (3/2)x + (7 + 4i)] = -4x^2 - (1/2)x - 7 - 4i
> (sub ad bs)
(polynomial x sparse-termlist 
  (term 2 (integer . -4))
  (term 1 (rational -1 . 2))
  (term 0 (complex rectangular (integer . -7) integer . -4)))
|#

#| Notes

Instead of "terms" and "polynomials" it could make sense to
enhance the "product" and "sum" types that already exist.

|#