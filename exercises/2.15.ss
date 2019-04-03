#|

Exercise 2.15: Eva Lu Ator, another user, has also noticed
the different intervals computed by different but
algebraically equivalent expressions. She says that a
formula to compute with intervals using Alyssa's system will
produce tighter error bounds if it can be written in such a
form that no variable that represents an uncertain number is
repeated. Thus, she says, "par2" is a "better" program for
parallel resistances than "par1". Is she right? Why?

|#

#| Answer

Everytime you do an operation against uncertain values, the
uncertaity may increase.

In par 1, you do one uncertain operation for the numerator,
one for the denominator, and one uncertain operation to
combine the terms for a total of three. 

In par 2, you divide one by an uncertain term twice (which
does not increase uncertainty) then you add the two
resultant terms to get the denominator (which increases
uncertainy) then you divide 1 by the denominator (which does
not increase uncertainy) for a total of one uncertain
operation.

Ensuring that each term appears once would reduce the number
of uncertainty operations but there will be a problem if
your algebra cannot be reduced to have each term appear only
once. For example f(x)=x^2 + x + 1, where x is an interval.

|#
