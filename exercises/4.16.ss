#|

Exercise 4.16: In this exercise we implement the method just
described for interpreting internal definitions. We assume
that the evaluator supports "let" (see Exercise 4.6).

a. Change "lookup-variable-value" (Section 4.1.3) to signal
an error if the value it finds is the symbol "*unassigned*".

b. Write a procedure "scan-out-defines" that takes a
procedure body and returns an equivalent one that has no
internal definitions, by making the transformation described
above.

c. Install "scan-out-defines" in the interpreter, either in
"make-procedure" or in "procedure-body" (see Section 4.1.3).
Which place is better? Why?

|#

