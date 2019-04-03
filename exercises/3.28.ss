#|

Exercise 3.28: Define an or-gate as a primitive function
box. Your "or-gate" constructor should be similar to
"and-gate".

|#

#| Answer |#
(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1))
      1
      0))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or (get-signal a1) (get-signal a2))])
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

#| Tests - not runnable |#
