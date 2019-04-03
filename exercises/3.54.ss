#|

Exercise 3.54: Define a procedure "mul-streams", analogous
to "add-streams", that produces the elementwise product of
its two input streams. Use this together with the stream of
"integers" to complete the following definition of the
stream whose n^(th) element (counting from 0) is n + 1
factorial:

(define factorials
  (cons-stream 1 (mul-streams <??> <??>)))

|#

(load-ex "3.53")

#| Answer 

Substituion can runs either direction :)

;;; assume f4 is (24 . (lambda () (mul-streams x y)))
(24 . (lambda () (mul-streams (stream-cdr x) (stream-cdr y))))
(24 . (lambda () (stream-map * (stream-cdr x) (stream-cdr y))))
(cons (* (stream-car x) (stream-car y))
      (lambda () (stream-map * (stream-cdr x) (stream-cdr y))))

~implies 24 is equal to~
(* (stream-car x) (stream-car y))
~which could be~
(* (stream-car f3) (stream-car i4))
~continuting with substitution~

(cons (* (stream-car f3) (stream-car i4))
      (lambda () (stream-map * (stream-cdr f3) (stream-cdr i4))))
(stream-map * f3 i4)
(mul-streams f3 i4)
((lambda () (mul-streams f3 i4)))
((cdr (<anything> . (lambda () (mul-streams f3 i4)))))

~we can guess f3 was~
(6 . (lambda () (mul-streams f3 i4)))

|#

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(define (factorial n)
  (stream-ref factorials (- n 1)))

#| Tests |#
(define-test (factorial 1) 1)
(define-test (factorial 2) 2)
(define-test (factorial 3) 6)
(define-test (factorial 4) 24)
(define-test (factorial 5) 120)
(define-test (factorial 16) 20922789888000)
