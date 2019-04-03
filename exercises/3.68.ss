#|

Exercise 3.68: Louis Reasoner thinks that building a stream
of pairs from three parts is unnecessarily complicated.
Instead of separating the pair (S_0, T_0) from the rest of
the pairs in the first row, he proposes to work with the
whole first row, as follows:

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

Does this work? Consider what happens if we evaluate "(pairs
integers integers)" using Louis's definition of "pairs".

|#

(load-ex "3.67")

#| Answer 

In order to invoke interleave, a recursive call to pairs in invoked. This
recurses infinitely for infinite streams s and t. If one or both streams is
finite it will eventually crash trying to cdr an empty stream.

|#
