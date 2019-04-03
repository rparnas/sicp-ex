#|

Exercise 3.70: It would be nice to be able to generate
streams in which the pairs appear in some useful order,
rather than in the order that results from an ad hoc
interleaving process. We can use a technique similar to the
"merge" procedure of Exercise 3.56, if we define a way to
say that one pair of integers is "less than" another. One
way to do this is to define a "weighting function" W(i, j)
and stipulate that (i_1, j_1) is less than (i_2, j_2) if
W(i_1, j_1) < W(i_2, j_2). Write a procedure
"merge-weighted" that is like "merge", except that
"merge-weighted" takes an additional argument "weight",
which is a procedure that computes the weight of a pair, and
is used to determine the order in which elements should
appear in the resulting merged stream. Using this,
generalize "pairs" to a procedure "weighted-pairs" that
takes two streams, together with a procedure that computes a
weighting function, and generates the stream of pairs,
ordered according to weight. Use your procedure to generate

a. the stream of all pairs of positive integers (i, j) with
i <= j ordered according to the sum i + j,

b. the stream of all pairs of positive integers (i, j) with
i <= j, where neither i nor j is divisible by 2, 3, or 5,
and the pairs are ordered according to the sum 2i + 3j +
5ij.

|#

(load-ex "3.69")

#| Answer |#
(define (merge-weighted s t W)
  (cond [(stream-null? s) t]
        [(stream-null? t) s]
        [else
         (let* ([s-car (stream-car s)]
                [t-car (stream-car t)]
                [s-weight (W s-car)]
                [t-weight (W t-car)])
           (cond [(< s-weight t-weight)
                  (cons-stream s-car (merge-weighted (stream-cdr s) t W))]
                 [else
                  (cons-stream t-car (merge-weighted s (stream-cdr t) W))]))]))

(define (weighted-pairs s t W)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) W)
      W)))

(define (370a-weight pair) 
  (let ([i (car pair)] [j (cadr pair)]) 
    (+ i j)))
(define 370a-stream (weighted-pairs integers integers 370a-weight))

(define (not-div-by-2-3-5 i)
  (not (or (= (remainder i 2) 0)
           (= (remainder i 3) 0)
           (= (remainder i 5) 0))))
(define (370b-weight pair)
  (let ([i (car pair)] [j (cadr pair)])
    (+ (* 2 i) (* 3 j) (* 5 i j))))
(define 370b-stream 
  (let ([not-div-2-3-5-stream (stream-filter not-div-by-2-3-5 integers)])
    (weighted-pairs not-div-2-3-5-stream not-div-2-3-5-stream 370b-weight)))

#| Tests |#

(define-test (map (lambda (i) (stream-ref 370a-stream i)) (iota 10))
             '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (2 4) (1 5) (3 4)))

(define-test (let ([weights (stream-map 370a-weight 370a-stream)])
               (andmap (lambda (i) (<= (stream-ref weights i)
                                       (stream-ref weights (+ i 1))))
                       (iota 1000)))
             #t)

(define-test (map (lambda (i) (stream-ref 370b-stream i)) (iota 10))
             '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7)))

(define-test (let ([weights (stream-map 370b-weight 370b-stream)])
               (andmap (lambda (i) (<= (stream-ref weights i)
                                       (stream-ref weights (+ i 1))))
                       (iota 1000)))
             #t)

#| Notes 

I'm still left confused by 3.67 which had us redefine pairs to generate all
pairs but now we are to assume "pairs" by default only means generating i <= j?

Also it's confusing to talk about ordering streams with this technique without
discussing its limitations. We're really just managing how the interleave is
performed. Obviously, we can't go through an infinite stream and fight the
element with the smallest weight.

What are the implications of merge sorting versus and absolute sorting?

Is a weight function where the first elements of both streams happen to have a
very high weight rather than a low weight still useful?

Do we need to know about the structure of the underlying streams in order to
determine which weighting functions will be useful?

|#
