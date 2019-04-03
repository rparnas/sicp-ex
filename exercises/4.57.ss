#|

Exercise 4.57: Define a rule that says that person 1 can
replace person 2 if either person 1 does the same job as
person 2 or someone who does person 1's job can also do
person 2's job, and if person 1 and person 2 are not the
same person. Using your rule, give queries that find the
following:

a. all people who can replace Cy D. Fect;

b. all people who can replace someone who is being paid more
than they are, together with the two salaries.

|#

(load-ex "4.56")

#| Answer |#
(set! test-db (append test-db '(
  (rule (can-replace-job ?job1 ?job2)
        (or (same ?job1 job2)
            (can-do-job ?job1 ?job2)
            (and (can-do-job ?job1 ?middle-job)
                 (can-replace-job ?middle-job ?job2))))
  (rule (can-replace ?p1 ?p2)
        (and (job ?p1 ?job1)
             (job ?p2 ?job2)
             (not (same ?p1 ?p2))
             (or (same ?job1 ?job2)
                 (can-replace-job ?job1 ?job2)))))))

#| Tests -- infrastructure |#
(define-test (do-query test-db '(can-replace-job ?j1 ?j2))
 '((can-replace-job job2 ?job2-1) 
   (can-replace-job (computer wizard) (computer programmer))
   (can-replace-job (computer wizard) (computer programmer trainee))
   (can-replace-job (computer wizard) (computer technician))
   (can-replace-job (computer programmer) (computer programmer trainee))
   (can-replace-job (administration secretary) (administration big wheel))))

(define-test (do-query test-db '(can-replace ?p1 ?p2))
  '((can-replace (Hacker Alyssa P) (Fect Cy D)) 
    (can-replace (Bitdiddle Ben) (Hacker Alyssa P))
    (can-replace (Fect Cy D) (Hacker Alyssa P))
    (can-replace (Bitdiddle Ben) (Fect Cy D))
    (can-replace (Bitdiddle Ben) (Tweakit Lem E))
    (can-replace (Bitdiddle Ben) (Reasoner Louis))
    (can-replace (Hacker Alyssa P) (Reasoner Louis))
    (can-replace (Fect Cy D) (Reasoner Louis))
    (can-replace (Aull DeWitt) (Warbucks Oliver))))

#| Tests |#

;;; a.
(define-test (do-query test-db '(and (job ?person . ?any0)
                                     (can-replace ?person (Fect Cy D))))
  '((and (job (Bitdiddle Ben) (computer wizard))
      (can-replace (Bitdiddle Ben) (Fect Cy D)))
    (and (job (Hacker Alyssa P) (computer programmer))
      (can-replace (Hacker Alyssa P) (Fect Cy D)))))

;;; b.
(define-test (do-query test-db
  '(and (salary ?p1 ?amount1)
        (salary ?p2 ?amount2)
        (lisp-value < ?amount1 ?amount2)
        (can-replace ?p1 ?p2)))
  '((and (salary (Fect Cy D) 35000)
         (salary (Hacker Alyssa P) 40000)
         (lisp-value < 35000 40000)
         (can-replace (Fect Cy D) (Hacker Alyssa P)))
    (and (salary (Aull DeWitt) 25000)
         (salary (Warbucks Oliver) 150000)
         (lisp-value < 25000 150000)
         (can-replace (Aull DeWitt) (Warbucks Oliver)))))