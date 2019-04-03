#|

Exercise 2.87: Install "=zero?" for polynomials in the
generic arithmetic package. This will allow "adjoin-term" to
work for polynomials with coefficients that are themselves
polynomials.

|#

(load-ex "2.86")

#| Code from the book -- loosely interpreted 
   same-variable -- copied from 2.56
   variable -- copied from 2.56
   added way to make terms
|#
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))
  ;; representation of terms and term lists
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
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in the same var: MUL-POLY" (list p1 p2))))
  (define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
    (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
    (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
    (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; Note: ambiguous how they expect you to make terms-lists.
;; Just hack the representation together manually because
;; the syntax is much cleaner than creating constructor
;; functions.

(install-polynomial-package)

#| Answer |#
(define (install-polynomial-=zero?)
  ;; ============================================
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (coeff term) (cadr term))
  ;; ============================================
  (put '=zero? '(polynomial) (lambda (p)
    (define (iter term-list)
      (cond [(null? term-list) #t]
            [(not (=zero? (coeff (first-term term-list)))) #f]
            [else (iter (rest-terms term-list))]))
    (iter (term-list p))))
  'done)

(install-polynomial-=zero?)

#| Tests |#
; x
(define-test (=zero? (make-polynomial 'x (list))) 
             #t)

; 0x^5
(define-test (=zero? (make-polynomial 'x (list (list 5 (make-integer 0)))))
             #t)

; (0y^5)(x^5)
(define-test (=zero? (make-polynomial 'x (list (list 5 (make-polynomial 'y (list (list 5 (make-integer 0))))))))
             #t)

; (x^5)
(define a (make-polynomial 'x (list (list 5 (make-integer 1)))))
(define-test (=zero? a)
             #f)
(define-test (add a a)
             '(polynomial x (5 (integer . 2))))
(define-test (mul a a)
             '(polynomial x (10 (integer . 1))))

; (y^5)(x^5)
(define b (make-polynomial 'x (list (list 5 (make-polynomial 'y (list (list 5 (make-integer 1))))))))
(define-test (=zero? b)
             #f)
(define-test (add b b)
             '(polynomial x (5 (polynomial y (5 (integer . 2))))))
