#|

Exercise 3.60: With power series represented as streams of
coefficients as in Exercise 3.59, adding series is
implemented by "add-streams". Complete the definition of the
following procedure for multiplying series:

(define (mul-series s1 s2)
  (cons-stream <??> (add-streams <??> <??>)))

You can test your procedure by verifying that sin^2x +
cos^2x = 1, using the series from Exercise 3.59.

|#

(load-ex "3.59")

#| Answer 

In general, multiplying power series x and y is:

0: (x0 * y0)
1: (x0 * y1) + (x1 * y0)
2: (x0 * y2) + (x1 * y1) + (x2 * y0)
3: (x0 * y3) + (x1 * y2) + (x2 * y1) + (x3 * y0)
4: (x0 * y4) + (x1 * y3) + (x2 * y2) + (x3 * y1) + (x4 * y0) 
5: (x0 * y5) + (x1 * y4) + (x2 * y3) + (x3 * y2) + (x4 * y1) + (x5 * y0)
. . .

Each column can be thought of as a stream that needs to be "added in".

The key to filling out the book's stubs is to notice that term 0 of the final
result stream aka (* (car x) (car y)) is not a "special case" but the first
element in a pattern: Term 0 in column 0 could be the head of the stream (scale-
stream y (car x)). The first term of this column arbitrarily has been calculated
already and must be skipped. Thus the book intends us to define mul-series like:

(define (mul-series x y)
  (cons-stream (* (stream-car x) (stream-car y))
               (add-streams (scale-stream (stream-cdr y) (stream-car x))
                            (mul-series (sctream-cdr x) y))))

However, this implementation doesn't match how I think about the domain. I'd
prefer:

(define (mul-series x y)
  (let ([column (scale-stream y (car x))])
    (cons-stream (stream-car column)
                 (add-streams (stream-cdr column)
                              (mul-series (stream-cdr x) y)))))

I believe this exercise was intended to take minutes but is unintentially
difficult because it fails to follow SICP's own advice. As experts in the domain
of powers series the book preparers mentally glossed over that thier
implementation doesn't perfectly match "how you'd like to talk about the
domain".

This exercise is a great negative example. When writing code to be read by
others, you must be cognizant of whether your code is easy for you to read
because it matches the domain or if you are getting away with a suboptimal match
because you internalized a transformation you perform mentally. In the latter
case you should document the transformation.

Lastly, if you consider the blanks in columns to be zeroes, you can simplify to:

|#

(define (mul-series x y)
    (add-streams (scale-stream y (car x))
                 (cons-stream 0 (mul-series (stream-cdr x) y))))

#| Tests |#

;;; (2x^2 + 3x + 4)^2 = (4x^4 + 12x^3 + 25x^2 + 24x + 16)
(define-test (let* ([s (list->stream '(4 3 2))]
                    [m (mul-series s s)])
               (list (stream-ref m 0)
                     (stream-ref m 1)
                     (stream-ref m 2)
                     (stream-ref m 3)
                     (stream-ref m 4)
                     (stream-ref m 5)))
               (list 16 24 25 12 4 0))

;;; sin^2(x) + cos^2(x) = 1
(define-test (let* ([s (add-streams (mul-series cosine-series cosine-series)
                                    (mul-series sine-series sine-series))])
               (list (stream-ref s 0)
                     (stream-ref s 1)
                     (stream-ref s 2)))
               (list 1 0 0))