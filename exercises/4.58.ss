#|

Exercise 4.58: Define a rule that says that a person is a
"big shot" in a division if the person works in the division
but does not have a supervisor who works in the division.

|#

(load-ex "4.57")

#| Answer |#
(set! test-db (append test-db '(
  (rule (is-big-shot ?person ?division)
        (and (job ?person (?division . ?any0) . ?any1)
             (or (not (supervisor ?person ?super))
                 (and (supervisor ?person ?super)
                      (not (job ?super (?division . ?any2) . ?any3)))))))))

#| Tests |#
(define-test (do-query test-db '(is-big-shot ?x ?y))
  '((is-big-shot (Warbucks Oliver) administration)
    (is-big-shot (Bitdiddle Ben) computer)
    (is-big-shot (Scrooge Eben) accounting)))