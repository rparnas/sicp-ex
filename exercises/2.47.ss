#|

Exercise 2.47: Here are two possible constructors for
frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

For each constructor supply the appropriate selectors to
produce an implementation for frames.

|#

#| Answer |#
(define (make-framea origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-framea f)
  (car f))
(define (edge1-framea f)
  (cadr f))
(define (edge2-framea f)
  (cddr f))

(define (make-frameb origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frameb f)
  (car f))
(define (edge1-frameb f)
  (cadr f))
(define (edge2-frameb f)
  (caddr f))

#| Tests |#

(define a (make-framea 'origin 'edge1 'edge2))
(define b (make-frameb 'origin 'edge1 'edge2))
(define-test (origin-framea a) 'origin)
(define-test (edge1-framea a) 'edge1)
(define-test (edge2-framea a) 'edge2)
(define-test (origin-frameb b) 'origin)
(define-test (edge1-frameb b) 'edge1)
(define-test (edge2-frameb b) 'edge2)
