#|

Exercise 4.72: Why do "disjoin" and "stream-flatmap"
interleave the streams rather than simply append them? Give
examples that illustrate why interleaving works better.
(Hint: Why did we use "interleave" in Section 3.5.3?)

|#

(load-ex "4.66") ; without loop detection
; (load-ex "4.71")

#| Answer

;;; from 4.67
(define (disjoin disjuncts frame-stream h)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
        (qeval (first-disjunct disjuncts) frame-stream h)
        (delay (disjoin (rest-disjuncts disjuncts) frame-stream h)))))
;;; from 4.55
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
        (stream-car stream)
        (delay (flatten-stream (stream-cdr stream))))))

A stream might have frames that are a class of solution. Interleaving rather
than appending ensures that one example from each class is explored early on in
the result stream. In the case of an infinite stream, interleaving ensures that
each class of solution is reachable.

For example, from the following database

(define inf-db '(
  (ones ())
  (twos ())
  (rule (ones (1 . ?x))
        (ones ?x))
  (rule (twos (2 . ?x))
        (twos ?x))))

You can perform a query which interleaves multiple infinite streams like

(or (ones ?x) (twos ?y))

|#
