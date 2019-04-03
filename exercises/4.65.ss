#|

Exercise 4.65: Cy D. Fect, looking forward to the day when
he will rise in the organization, gives a query to find all
the wheels (using the "wheel" rule of Section 4.4.1):

(wheel ?who)

To his surprise, the system responds

 ;;; Query results: 
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))

Why is Oliver Warbucks listed four times?

|#

(load-ex "4.64")

#| Answer 

The rule for wheel from 4.55:

  (rule (wheel ?person)
    (and (supervisor ?middle-manager ?person)
         (supervisor ?x ?middle-manager)))

The rule doesn't implement "who is a wheel". It implements "match every
supervisor relationship against every supervisor relationship and keep matches
where the person being managed in the first relationship is a manager in the
second."

The query language itself is deceptive because it looks like it expresses the
logic queries the user wishes to solve but in reality it describes a procedure
and to get the correct mathematical logic answer the user must do manual
processing. In the example the user must interpret the results as "Ben and
Oliver are wheels".

|#

#| Tests |#
(define-test (do-query test-db 
  '(and (supervisor ?middle-manager ?person)
        (supervisor ?x ?middle-manager)))
  '((and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
         (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
    (and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
         (supervisor (Reasoner Louis) (Hacker Alyssa P)))
    (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
         (supervisor (Fect Cy D) (Bitdiddle Ben)))
    (and (supervisor (Scrooge Eben) (Warbucks Oliver))
         (supervisor (Cratchet Robert) (Scrooge Eben)))
    (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
         (supervisor (Tweakit Lem E) (Bitdiddle Ben)))))