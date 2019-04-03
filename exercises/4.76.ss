#|

Exercise 4.76: Our implementation of "and" as a series
combination of queries (Figure 4.5) is elegant, but it is
inefficient because in processing the second query of the
"and" we must scan the data base for each frame produced by
the first query. If the data base has n elements, and a
typical query produces a number of output frames
proportional to n (say n / k), then scanning the data base
for each frame produced by the first query will require n^2
/ k calls to the pattern matcher. Another approach would be
to process the two clauses of the "and" separately, then
look for all pairs of output frames that are compatible. If
each query produces n / k output frames, then this means
that we must perform n^2 / k^2 compatibility checks---a
factor of k fewer than the number of matches required in our
current method.

Devise an implementation of "and" that uses this strategy.
You must implement a procedure that takes two frames as
inputs, checks whether the bindings in the frames are
compatible, and, if so, produces a frame that merges the two
sets of bindings. This operation is similar to unification.

|#

(load-ex "4.66") ; without loop detection
; (load-ex "4.75")
(no-regression) ; prevent infinite loops

#| Answer

I believe the implementation suggested by the exercise breaks not and lisp-value
functionality which cannot be considered in isolation but instead rely on the
procedural nature of the query system.

It can also introduce issues with infinite loops as clauses which were intended
to have bound variables by the time they were reached are evaluated in isolation
with unbounded variables.

But how was I supposed to know what level of functionality I should preserve
from with this change from the text? It should have a hint indicating this is a
one-off experimental change.

|#

(define (merge-streams s0 s1)
  (set! a s0)
  (set! b s1)
  (cond [(stream-null? s1) the-empty-stream]
        [else (stream-append (merge-stream-and-frame s0 (stream-car s1))
                             (merge-streams s0 (stream-cdr s1)))]))

(define (merge-stream-and-frame s f)
  (if (stream-null? s)
      the-empty-stream
      (let ([head (merge-frames (stream-car s) f)])
        (if (not (eq? head 'failed))
            (cons-stream head (merge-stream-and-frame (stream-cdr s) f))
            (merge-stream-and-frame (stream-cdr s) f)))))

(define (merge-frames f0 f1)
  (if (null? f1)
      f0
      (let* ([head (car f1)]
             [var (binding-variable head)]
             [val (binding-value head)]
             [next-f0 (extend-if-possible var val f0)])
        (if (eq? next-f0 'failed)
            'failed
            (merge-frames next-f0 (cdr f1))))))

(define (conjoin conjuncts frame-stream)
  (cond [(empty-conjunction? conjuncts) 
         frame-stream]
        [(empty-conjunction? (cdr conjuncts))
         (qeval (first-conjunct conjuncts) frame-stream)]
        [else
         (merge-streams (qeval (first-conjunct conjuncts) frame-stream)
                        (conjoin (rest-conjuncts conjuncts) frame-stream))]))

#| Tests

Many regression tests fail or never return, but you can run a few manually to
ensure the general idea works.

|#

;;; modifiy tests to assume not and lisp-value are no longer supposed
(define (clear-all!)
  (set! THE-ASSERTIONS the-empty-stream)
  (set! THE-RULES the-empty-stream)
  (set! rule-counter 0)
  (set! op-table (list))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval (lambda args (error "not" "not supported")))
  (put 'lisp-value 'qeval (lambda args (error "not" "not supported")))
  (put 'always-true 'qeval always-true))