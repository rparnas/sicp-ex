#|

Exercise 5.51: Develop a rudimentary implementation of
Scheme in C (or some other low-level language of your
choice) by translating the explicit-control evaluator of
Section 5.4 into C. In order to run this code you will need
to also provide appropriate storage-allocation routines and
other run-time support.

|#

#| Answer 

The C evaluator is based on 4.1 ~ 4.7 and 5.32. It is in the repl folder. Below
I thought about some of the major architectural things that would need to happen
in order to make a complete well-implemented scheme in C.

Of note is "let*->nested-lets". This procedure is implemented in scheme and
injected into the default environment. This proof of concept opens the gateway
to implementing large swaths of the interpreter in scheme.

* eval and apply need to be implemented. I don't think any evaluator in the book
supports eval or apply syntax. This seems important for implementing things like
map which rely procedures which take an arbitrary number of arguments.

* There are many cases where I want functions of arbitrary variables or overload
in C to be more symmetric to scheme.

* Manipulating s-expressions in C is a bit messy. The s-expression manipulating
functions return s-expressions, but sometimes we want to cross over to C, i.e.
convert an s-expression result to a C integer. Perhaps this messiness is fine if
most of the places this hapenns can instead be implemented directly in scheme.

* Implementation is needed for different numeric types, and also determining
which primitive values should be represented as the same memory location.

* The garbage collector isn't ideal. Saving all registers to the stack as a
way to mark which things are in use is elegant however this limits our ability
to execute garbage collection at arbitrary times.

|#