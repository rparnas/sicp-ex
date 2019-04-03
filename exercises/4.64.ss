#|

Exercise 4.64: Louis Reasoner mistakenly deletes the
"outranked-by" rule (Section 4.4.1) from the data base. When
he realizes this, he quickly reinstalls it. Unfortunately,
he makes a slight change in the rule, and types it in as

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person
                           ?middle-manager))))

Just after Louis types this information into the system,
DeWitt Aull comes by to find out who outranks Ben Bitdiddle.
He issues the query

(outranked-by (Bitdiddle Ben) ?who)

After answering, the system goes into an infinite loop.
Explain why.

|#

(load-ex "4.63")

#| Answer

;;; Original
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
;;; New
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

In the original query, ?middle-manager is bound by the first term of and. That
?middle-manager value is bound to ?staff-person in the a recursive outranked-by.
Assuming no circular references, eventually you will reach some (supervisor
?staff-person ?middle-manager) with no values, short circuiting any additional
recursive processing.

In the new query, the first term of and, (outranked-by ?middle-manager ?boss) is
processed with nothing bound. This recursion will never end.

This query relies on the short-circuiting behavior of and to implement its base
case.

|#
