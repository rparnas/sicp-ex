#|

Exercise 2.59: Implement the "union-set" operation for the
unordered-list representation of sets.

|#

(define false #f)
(define true #t)

#| Code from Book |#
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

#| Answer |#
(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(not (element-of-set? (car set1) set2))
          (cons (car set1) (union-set (cdr set1) set2))]
        [else (union-set (cdr set1) set2)]))

#| Tests |#
(define-test (union-set '(1 2 3) '(3 4 5)) '(1 2 3 4 5))
