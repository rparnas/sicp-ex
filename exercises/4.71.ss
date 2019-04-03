#|

Exercise 4.71: Louis Reasoner wonders why the "simple-query"
and "disjoin" procedures (Section 4.4.4.2) are implemented
using explicit "delay" operations, rather than being defined
as follows:

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append
      (find-assertions query-pattern frame)
      (apply-rules query-pattern frame)))
   frame-stream))
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts)
              frame-stream)
       (disjoin (rest-disjuncts disjuncts)
                frame-stream))))

Can you give examples of queries where these simpler
definitions would lead to undesirable behavior?

|#

; (load-ex "4.66") ; without loop detection
(load-ex "4.69") ; with loop detection

#| Code from book -- modified with history |#
(define (simple-query query-pattern frame-stream h)
  (stream-flatmap
   (lambda (frame)
     (stream-append
      (find-assertions query-pattern frame)
      (apply-rules query-pattern frame h)))
   frame-stream))
(define (disjoin disjuncts frame-stream h)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts)
              frame-stream h)
       (disjoin (rest-disjuncts disjuncts)
                frame-stream h))))

#| Other code |#
(define (interleave s0 s1)
  (cond [(stream-null? s0) s1]
        [(stream-null? s1) s0]
        [(cons-stream (stream-car s0) (interleave s1 (stream-cdr s0)))]))

#| Answer

In simple-query delay is nice in cases where the entire stream isn't needed such
as for negation. In disjoin it is nice for short-circuiting.

If the system does not have loop detection, the delayed arguments provide a way
to at least obtain some results. In the case of disjoin, short-circuiting is
might be relied upon to avoid infinite looping entirely.

NOTE: This question is confusing as 4.67 had us implement loop detection
rendering the above updates tolerable (all regression tests pass).

|#

#| Tests

> (define db471a '(
    (a 0)
    (rule (a ?x) 
          (a ?x))))
> (do-query db471a '(a 0))
((a 0) (a 0))

> (define db471b '(
    (a 0)
    (rule (z ?x) 
          (or (a ?x)
              (z ?x)))))
> (do-query db471b '(z 0))
((z 0))

|#