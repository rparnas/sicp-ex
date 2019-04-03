#|

Exercise 5.19: Alyssa P. Hacker wants a breakpoint feature
in the simulator to help her debug her machine designs. You
have been hired to install this feature for her. She wants
to be able to specify a place in the controller sequence
where the simulator will stop and allow her to examine the
state of the machine. You are to implement a procedure

(set-breakpoint <machine> <label> <n>)

that sets a breakpoint just before the n^(th) instruction
after the given label. For example,

(set-breakpoint gcd-machine 'test-b 4)

installs a breakpoint in "gcd-machine" just before the
assignment to register "a". When the simulator reaches the
breakpoint it should print the label and the offset of the
breakpoint and stop executing instructions. Alyssa can then
use "get-register-contents" and "set-register-contents!" to
manipulate the state of the simulated machine. She should
then be able to continue execution by saying

(proceed-machine <machine>)

She should also be able to remove a specific breakpoint by
means of

(cancel-breakpoint <machine> <label> <n>)

or to remove all breakpoints by means of

(cancel-all-breakpoints <machine>)

|#

(load-ex "5.18")

#| Answer -- added to 5.15 |#

#| Tests 

> (define (m519) (make-machine (list) '(one (assign a (const 1)) two (assign a (const 2)) three (assign a (const 3)))))))
> (define m (m519))
> (trace-on)
> (set-breakpoint m 'two 1)
> (start m)
one
(assign a (const 1))
two
break
> (proceed m)
(assign a (const 2))
three
(assign a (const 3))

|#