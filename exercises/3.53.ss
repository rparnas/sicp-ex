#|

Exercise 3.53: Without running the program, describe the
elements of the stream defined by

(define s (cons-stream 1 (add-streams s s)))

|#

#| Code from book |#
(load-ex "3.52")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

#| Answer

;;; s1 is (1 . (lambda () (add-streams s1 s1))) => 1

;;; s2 is ((cdr s1))
((cdr (1 . (lambda () (add-streams s1 s1)))))
((lambda () (add-streams s1 s1)))
(add-streams s1 s1)
(stream-map + s1 s1)
(cons (+ (stream-car s1) (stream-car s1))
      (lambda () (stream-map + (stream-cdr s1) (stream-cdr s1))))
(cons (+ (car (1 . (lambda () (add-streams s1 s1)))) 
         (car (1 . (lambda () (add-streams s1 s1)))))
      (lambda () (stream-map + (stream-cdr s1) (stream-cdr s1))))
(2 . (lambda () (stream-map + (stream-cdr s1) (stream-cdr s1))))
(2 . (lambda () (add-streams (stream-cdr s1) (stream-cdr s1))))
(2 . (lambda () (add-streams s2 s2)))
;;; s2 is (2 . (lambda () (add-streams s2 s2))) => 2

The same transformations can be applied to any s_n to get s_(n+1).

The sequence is powers of two.

|#