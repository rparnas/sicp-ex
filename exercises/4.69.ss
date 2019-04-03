#|

Exercise 4.69: Beginning with the data base and the rules
you formulated in Exercise 4.63, devise a rule for adding
"greats" to a grandson relationship. This should enable the
system to deduce that Irad is the great-grandson of Adam, or
that Jabal and Jubal are the
great-great-great-great-great-grandsons of Adam. (Hint:
Represent the fact about Irad, for example, as "((great
grandson) Adam Irad)". Write rules that determine if a list
ends in the word "grandson". Use this to express a rule that
allows one to derive the relationship "((great. ?rel) ?x
?y)", where "?rel" is a list ending in "grandson".) Check
your rules on queries such as "((great grandson) ?g ?ggs)"
and "(?relationship Adam Irad)".

|#

(load-ex "4.68")

#| Answer |#
(define db469 (append db463 '(
  (rule (ends-with (?x) ?x))
  (rule (ends-with (?head . ?tail) ?x)
        (ends-with ?tail ?x))
  (rule (relationship (son) ?s ?f)
        (son-of ?s ?f))
  (rule (relationship (grandson) ?s ?g)
        (grandson-of ?s ?g))
  (rule (relationship (great . ?rel) ?descendant ?ancestor)
        (and (son-of ?descendant ?father)
             (relationship ?rel ?father ?ancestor)
             (ends-with ?rel grandson))))))

#| Tests -- infrastructure |#
(define-test (do-query db469 
  '(ends-with (grandson) grandson))
  '((ends-with (grandson) grandson)))
(define-test (do-query db469 
  '(ends-with (great grandson) grandson))
  '((ends-with (great grandson) grandson)))

#| Tests |#
(define-test (do-query db469
  '(relationship (great grandson) ?g ?ggs))
  '((relationship (great grandson) Jabal Mehujael) 
    (relationship (great grandson) Jubal Mehujael)
    (relationship (great grandson) Irad Adam)
    (relationship (great grandson) Mehujael Cain)
    (relationship (great grandson) Methushael Enoch)
    (relationship (great grandson) Lamech Irad)))

(define-test (do-query db469
  '(relationship ?relationship Irad Adam))
  '((relationship (great grandson) Irad Adam)))
