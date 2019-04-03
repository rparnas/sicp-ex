#|

Exercise 4.19: Ben Bitdiddle, Alyssa P. Hacker, and Eva Lu
Ator are arguing about the desired result of evaluating the
expression

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

Ben asserts that the result should be obtained using the
sequential rule for "define": "b" is defined to be 11, then
"a" is defined to be 5, so the result is 16. Alyssa objects
that mutual recursion requires the simultaneous scope rule
for internal procedure definitions, and that it is
unreasonable to treat procedure names differently from other
names. Thus, she argues for the mechanism implemented in
Exercise 4.16. This would lead to "a" being unassigned at
the time that the value for "b" is to be computed. Hence, in
Alyssa's view the procedure should produce an error. Eva has
a third opinion. She says that if the definitions of "a" and
"b" are truly meant to be simultaneous, then the value 5 for
"a" should be used in evaluating "b". Hence, in Eva's view
"a" should be 5, "b" should be 15, and the result should be
20. Which (if any) of these viewpoints do you support? Can
you devise a way to implement internal definitions so that
they behave as Eva prefers?

|#

#| Answer 

sequential definitions -- local-b= (+ outer-a x)
                          local-a=5
                          returns (+ local-a local-b)

hoisted definitions -- local-a=undefined
                       local-b=undefined
                       local-b= (+ local-a x) ==> error

simultaneous definitions -- local-a=5
                            local-b= (+ local-a x)
                            returns (+ local-a local-b)

In order to support a block of simultaneous definitions you can analyze all
definitions to determine dependencies and declare things in the correct order.

I'm most concerned with name collision issues. In recent C-like languages every
symbol can be addresssed uniquely and if there is an ambiguity it is considered
invalid syntax. What would it take to get that kind of safety here in scheme?

By default I'd support the approach that my version of scheme uses which is
something like "forced manual hoisting" in which the top of the body is the only
valid context for define statements.

This avoids the "which thing named x am I accessing problem" later in the body
even though it is still present in the definition block. Definitions are still
sequentially executed keeping things more WYSIWIG compared with automated
hoisting.

You could also try out an extension to this: prevent the user from accessing a
synbol in a define if the name of that symbol will later be shadowed. See also:
What problems have they had in JavaScript the last decades.

|#
