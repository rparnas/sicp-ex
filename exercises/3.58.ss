#|

Exercise 3.58: Give an interpretation of the stream computed
by the following procedure:

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

("quotient" is a primitive that returns the integer quotient
of two integers.) What are the successive elements produced
by "(expand 1 7 10)"? What is produced by "(expand 3 8 10)"?

|#

(load-ex "3.57")

#| Code from book |#
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
      (expand (remainder (* num radix) den) den radix)))

#| Answer 

den and radix do not change.

(expand 1 7 10)
(e 1 ...) => (/ 10 7) => (1 r 3) => (1 ...
(e 3 ...) => (/ 30 7) => (4 r 2) => (4 ...
(e 2 ...) => (/ 20 7) => (2 r 6) => (2 ...
(e 6 ...) => (/ 60 7) => (8 r 4) => (8 ...
(e 4 ...) => (/ 40 7) => (5 r 5) => (5 ...
(e 5 ...) => (/ 50 7) => (7 r 1) => (7 ...
(e 1 ...) . . .
yields the repeating sequence (1 3 2 6 4 5)

(expand 3 8 10) 
(e 3 ...) => (/ 30 8) => (3 r 6)
(e 6 ...) => (/ 60 8) => (7 r 4)
(e 4 ...) => (/ 40 8) => (5 r 0)
(e 0 ...) => (/ 0 8) => (0 r 0)
yields the sequence (3 7 5) followed by infinite zeroes

Calling expand with radix 10 computes the decimal point digits of long divison.

This could be turned into a long division algorithm, for example:

|#

(define (long-division-to-six-places dividend divisor)
  (let* ([q (quotient dividend divisor)]
         [r (remainder dividend divisor)]
         [s (expand r divisor 10)])
    (if (= r 0)
        (list q)
        (list q "."
                (stream-ref s 0)
                (stream-ref s 1)
                (stream-ref s 2)
                (stream-ref s 3)
                (stream-ref s 4)
                (stream-ref s 5)))))
