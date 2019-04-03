#|

Exercise 2.89: Define procedures that implement the
term-list representation described above as appropriate for
dense polynomials.

|#

#| Answer 

This is leading towards a rewrite in 2.90 thus this just
strictly implement the dense termlist. This is a bad
representation because it excludes the ability to represent
x^-1 for no good reason.

|#

(define (=zero? x) (= 0 x)) ; hack for now

(define (the-empty-termlist) '())
(define (first-term term-list) 
  (make-term (- (length term-list) 1) (car term-list)))
(define (rest-terms term-list) 
  (memp (lambda (x) (not (= x 0))) (cdr term-list)))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (adjoin-term term term-list)
  (define (zeros n)
    (define (iter result n)
      (if (= 0 n)
          result
          (iter (cons 0 result) (- n 1))))
    (iter '() n))
  (if (=zero? (coeff term))
      term-list
      (let ([ft-order (if (empty-termlist? term-list)
                          -1
                          (order (first-term term-list)))])
        (if (< (order term) ft-order)
            (error "adjoin-term" 
                   "must build up list in order"
                   term term-list))
        (append (list (coeff term)) 
                (zeros (- (order term) ft-order 1)) 
                term-list))))

#| Tests |#

; x^6 + 3x^4 + 4x^3 + 6x^2 + 2
(define test 
  (adjoin-term (make-term 6 1)
    (adjoin-term (make-term 4 3)
      (adjoin-term (make-term 3 4)
        (adjoin-term (make-term 2 6)
          (adjoin-term (make-term 0 2) 
            (the-empty-termlist)))))))
(define-test test 
             '(1 0 3 4 6 0 2))
(define-test (first-term test)
             '(6 1))
(define-test (first-term (rest-terms test))
             '(4 3))
(define-test (first-term (rest-terms (rest-terms test)))
             '(3 4))
(define-test (first-term (rest-terms (rest-terms (rest-terms test))))
             '(2 6))
(define-test (first-term (rest-terms (rest-terms (rest-terms (rest-terms test)))))
             '(0 2))

; > (adjoin-term (make-term 0 1) '(1 2 3))
; Exception in adjoin-term: must build up list in order with irritants ((0 1) (1 2 3))
; Type (debug) to enter the debugger.
