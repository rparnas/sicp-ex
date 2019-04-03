#|

Exercise 4.46: The evaluators in Section 4.1 and Section 4.2
do not determine what order operands are evaluated in. We
will see that the "amb" evaluator evaluates them from left
to right. Explain why our parsing program wouldn't work if
the operands were evaluated in some other order.

|#

#| Answer 

This system relies on right-to-left order because parse-word implicitly means to
check the rightmost element from the unparsed phrase.

For example "(some-function (parse-word articles) (parse-word nouns))" intends
to check that the rightmost element is an article and the next rightmost element
is a noun before passing those words to some-function.

What if parse-word was implemented in such a way that what element it looks at
matched the direction the evaluator? This is possible but I think it is
redefining the meaning of parse-word.

|#