#|

Exercise 2.33: Fill in the missing expressions to complete
the following definitions of some basic list-manipulation
operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) <??>) nil sequence))
(define (append seq1 seq2)
  (accumulate cons <??> <??>))
(define (length sequence)
  (accumulate <??> 0 sequence))

|#

#| Code from book |#
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

#| Answer |#
(define (map0 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append0 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length0 seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

#| Tests |#
(define ls '(1 2 3 4 5))
(define-test (map (lambda (x) (+ 2 x)) ls) '(3 4 5 6 7))
(define-test (map0 (lambda (x) (+ 2 x)) ls) '(3 4 5 6 7))
(define-test (append ls ls) '(1 2 3 4 5 1 2 3 4 5))
(define-test (append0 ls ls) '(1 2 3 4 5 1 2 3 4 5))
(define-test (length ls) 5)
(define-test (length0 ls) 5)