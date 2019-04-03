#|

Exercise 4.56: Formulate compound queries that retrieve the
following information:

a. the names of all people who are supervised by Ben
Bitdiddle, together with their addresses;

b. all people whose salary is less than Ben Bitdiddle's,
together with their salary and Ben Bitdiddle's salary;

c. all people who are supervised by someone who is not in
the computer division, together with the supervisor's name
and job.

|#

(load-ex "4.55")

#| Answer -- see tests below |#

#| Tests |#

;;; a.
(define-test (do-query test-db '(and (supervisor ?x (Bitdiddle Ben))
                                     (address ?x . ?details)))
  '((and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
      (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
    (and (supervisor (Fect Cy D) (Bitdiddle Ben))
         (address (Fect Cy D) (Cambridge (Ames Street) 3)))
    (and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
         (address (Tweakit Lem E) (Boston (Bay State Road) 22)))))
;;; b.
(define-test (do-query test-db
  '(and (salary (Bitdiddle Ben) ?bamount)
        (salary ?person ?pamount)
        (lisp-value < ?pamount ?bamount)))
  '((and (salary (Bitdiddle Ben) 60000)
         (salary (Hacker Alyssa P) 40000)
         (lisp-value < 40000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Fect Cy D) 35000)
         (lisp-value < 35000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Tweakit Lem E) 25000)
         (lisp-value < 25000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Reasoner Louis) 30000)
         (lisp-value < 30000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Cratchet Robert) 18000)
         (lisp-value < 18000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Aull DeWitt) 25000)
         (lisp-value < 25000 60000))))
;;; c.
(define-test (do-query test-db
  '(and (supervisor ?person ?super)
        (job ?super . ?any0)
        (not (job ?super (computer . ?any1)))))
  '((and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
         (job (Warbucks Oliver) (administration big wheel))
         (not (job (Warbucks Oliver) (computer . ?any1))))
    (and (supervisor (Scrooge Eben) (Warbucks Oliver))
         (job (Warbucks Oliver) (administration big wheel))
         (not (job (Warbucks Oliver) (computer . ?any1))))
    (and (supervisor (Cratchet Robert) (Scrooge Eben))
         (job (Scrooge Eben) (accounting chief accountant))
         (not (job (Scrooge Eben) (computer . ?any1))))
    (and (supervisor (Aull DeWitt) (Warbucks Oliver))
         (job (Warbucks Oliver) (administration big wheel))
         (not (job (Warbucks Oliver) (computer . ?any1))))))