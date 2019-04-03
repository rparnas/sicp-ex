#|

Exercise 2.16: Explain, in general, why equivalent algebraic
expressions may lead to different answers. Can you devise an
interval-arithmetic package that does not have this
shortcoming, or is this task impossible? (Warning: This
problem is very difficult.)

|#

#| Answer 

I think this has to do with what the intervals you are
operating on represent. What is meant by "uncertainty"? In
this approach including a term twice in an algebraic
expression may represent "taking two uncertain and
independent measurements" but in real life perhaps you only
took one measurement.

See also: the dependency problem of interval arithmetic.

|#
