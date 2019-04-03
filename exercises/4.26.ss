#|

Exercise 4.26: Ben Bitdiddle and Alyssa P. Hacker disagree
over the importance of lazy evaluation for implementing
things such as "unless". Ben points out that it's possible
to implement "unless" in applicative order as a special
form. Alyssa counters that, if one did that, "unless" would
be merely syntax, not a procedure that could be used in
conjunction with higher-order procedures. Fill in the
details on both sides of the argument. Show how to implement
"unless" as a derived expression (like "cond" or "let"), and
give an example of a situation where it might be useful to
have "unless" available as a procedure, rather than as a
special form.

|#

#| Answer 

The unless syntax...

  (unless condition usual-value exceptional-value)

can be implemented as the derived expression:

  (if condition exceptional-value usual-value))

Unless would be useful as a first-class procedure like

  (map unless <conditions> <usual-values> <exceptional-values>)

Even though in an applicative order the values wouldn't be lazily evaluated it
could still be useful to have the concept of "unless" available as a procedure.

But in an applicative-order language you should probably pick whether the name
"unless" means lazy evaluation or if it means some other concept of "unless",
having one thing that does both seems like it can only be elegantly embodied in
a normal-order language.

|#