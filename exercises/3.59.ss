#|

Exercise 3.59: In Section 2.5.3 we saw how to implement a
polynomial arithmetic system representing polynomials as
lists of terms. In a similar way, we can work with power
series, such as

               x^2     x^3       x^4
e^x = 1 + x + ----- + ----- + --------- + ...
                2     3 * 2   4 * 3 * 2

             x^2       x^4
cos x = 1 - ----- + --------- - ...
              2     4 * 3 * 2

             x^3         x^5
sin x = x - ----- + ------------- - ...
            3 * 2   5 * 4 * 3 * 2

represented as infinite streams. We will represent the
series a_0 + a_1 x + a_2 x^2 + a_3 x^3 +... as the stream
whose elements are the coefficients a_0, a_1, a_2, a_3,....

a. The integral of the series a_0 + a_1 x + a_2 x^2 + a_3
x^3 +... is the series

             1             1             1
c + a_0 x + --- a_1 x^2 + --- a_2 x^3 + --- a_3 x^4 + ...
             2             3             4

where c is any constant. Define a procedure
"integrate-series" that takes as input a stream a_0, a_1,
a_2,... representing a power series and returns the stream
a_0, (1/2)a_1, (1/3)a_2,... of coefficients of the
non-constant terms of the integral of the series. (Since the
result has no constant term, it doesn't represent a power
series; when we use "integrate-series", we will "cons" on
the appropriate constant.)

b. The function x |--> e^x is its own derivative. This
implies that e^x and the integral of e^x are the same
series, except for the constant term, which is e^0 = 1.
Accordingly, we can generate the series for e^x as

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

Show how to generate the series for sine and cosine,
starting from the facts that the derivative of sine is
cosine and the derivative of cosine is the negative of sine:

(define cosine-series (cons-stream 1 <??>))
(define sine-series (cons-stream 0 <??>))

|#

#| Answer |#
(load-ex "3.58")

;;; a. 

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series a)
  (div-streams a integers))

(define zeroes (cons-stream 0 zeroes))

(define (list->stream ls)
  (if (null? ls)
      zeroes
      (cons-stream (car ls) (list->stream (cdr ls)))))

;;; b.

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;;;; the derivative of sine is cosine
;;;; the derivative of cosine is the negative of sine
;;;; e^x = e0 + e1 + e2 + e3 + e4 + e5 + ...
;;;; sin =      e1      - e3      + e5 - ...
;;;; cos = e0      - e2      + e4      - ...

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))


#| Tests |#

;;; integral of 1 + 2x + 9x^2 + 8x^3 + 10x^4 + 12x^5 dx is 
;;;             c + x + x^2 + 3x^3 + 2x^4 + 2x^5 + 2x^6
(define-test (let* ([a (list->stream '(1 2 9 8 10 12))]
                    [i (integrate-series a)])
               (list (stream-ref i 0)
                     (stream-ref i 1)
                     (stream-ref i 2)
                     (stream-ref i 3)
                     (stream-ref i 4)
                     (stream-ref i 5)
                     (stream-ref i 6)))
               (list 1 1 3 2 2 2 0))

(define-test (list (stream-ref cosine-series 0)
                   (stream-ref cosine-series 1)
                   (stream-ref cosine-series 2)
                   (stream-ref cosine-series 3)
                   (stream-ref cosine-series 4))
             (list (stream-ref exp-series 0)
                   0
                   (- (stream-ref exp-series 2))
                   0
                   (stream-ref exp-series 4)))

(define-test (list (stream-ref sine-series 0)
                   (stream-ref sine-series 1)
                   (stream-ref sine-series 2)
                   (stream-ref sine-series 3)
                   (stream-ref sine-series 4))
             (list 0
                   (stream-ref exp-series 1)
                   0
                   (- (stream-ref exp-series 3))
                   0))
