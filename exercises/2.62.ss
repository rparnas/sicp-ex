#|

Exercise 2.62: Give a Theta(n) implementation of "union-set"
for sets represented as ordered lists.

|#

#| Code from book |#

; Theta(n) implementation of intersection-set for ordered
; lists.
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(load-ex "2.61")

(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let ([x1 (car set1)]
                    [x2 (car set2)])
  (cond [(= x1 x2)
         (cons x1 (union-set (cdr set1) (cdr set2)))]
        [(< x1 x2)
         (cons x1 (union-set (cdr set1) set2))]
        [(< x2 x1)
         (cons x2 (union-set set1 (cdr set2)))]))]))

#| Tests |#
(define-test (union-set '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))
