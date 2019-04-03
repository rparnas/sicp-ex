#|

Exercise 4.28: "eval" uses "actual-value" rather than "eval"
to evaluate the operator before passing it to "apply", in
order to force the value of the operator. Give an example
that demonstrates the need for this forcing.

|#

#| Answer 

eval is the interface for evaluating scheme expressions. A thunk is not a scheme
expression. It is an internal data-structure representing a scheme expression
whose evaluation is to be delayed as long as possible.

When an application occurrs the operator is needed. That is the moment from
which it can be delayed no longer.

For example: At ((proc-which-returns-proc) arg0...) the operator could be an
unevaluated thunk that needs to be unpacked, or it could be an evaluated thunk
that needs to be unpacked. Neither of these are scheme expressions.

|#