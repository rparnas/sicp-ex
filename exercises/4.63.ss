#|

Exercise 4.63: The following data base (see Genesis 4)
traces the genealogy of the descendants of Ada back to Adam,
by way of Cain:

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

Formulate rules such as "If S is the son of f, and f is the
son of G, then S is the grandson of G" and "If W is the wife
of M, and S is the son of W, then S is the son of M" (which
was supposedly more true in biblical times than today) that
will enable the query system to find the grandson of Cain;
the sons of Lamech; the grandsons of Methushael. (See
Exercise 4.69 for some rules to deduce more complicated
relationships.)

|#

(load-ex "4.62")

; If S is the son of f, and f is the son of G, then S is the grandson of G
; If W is the wife of M, and S is the son of W, then S is the son of M
; the grandson of Cain
; the sons of Lamech
; the grandsons of Methushael.

#| Answer |#
(define db463 '(
  ;;; assertions
  (son Adam Cain)
  (son Cain Enoch)
  (son Enoch Irad)
  (son Irad Mehujael)
  (son Mehujael Methushael)
  (son Methushael Lamech)
  (wife Lamech Ada)
  (son Ada Jabal)
  (son Ada Jubal)
  ;;; rules
  (rule (son-of ?s ?f)
        (or (son ?f ?s)
            (and (wife ?f ?w)
                 (son ?w ?s))))
  (rule (grandson-of ?s ?g)
        (and (son-of ?s ?f)
             (son-of ?f ?g)))))

#| Tests |#

;;; the grandson of Cain -- Irad
(define-test (do-query db463
  '(grandson-of ?s Cain))
  '((grandson-of Irad Cain)))

;;; the sons of Lamech -- Javal and Jubal
(define-test (do-query db463
  '(son-of ?s Lamech))
  '((son-of Jabal Lamech) 
    (son-of Jubal Lamech)))

;;; the grandsons of Methushael -- Javal and Jubal
(define-test (do-query db463
  '(grandson-of ?s Methushael))
  '((grandson-of Jabal Methushael) 
    (grandson-of Jubal Methushael)))