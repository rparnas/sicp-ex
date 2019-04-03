#|

Exercise 4.15: Given a one-argument procedure "p" and an
object "a", "p" is said to "halt" on "a" if evaluating the
expression "(p a)" returns a value (as opposed to
terminating with an error message or running forever). Show
that it is impossible to write a procedure "halts?" that
correctly determines whether "p" halts on "a" for any
procedure "p" and object "a". Use the following reasoning:
If you had such a procedure "halts?", you could implement
the following program:

(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p) (run-forever) 'halted))

Now consider evaluating the expression "(try try)" and show
that any possible outcome (either halting or running
forever) violates the intended behavior of "halts?".

|#

#| Answer 

Assume it is possible to write halts?

if (try try) halts this implies (halts? try try) is true.
This contradicts that if (halts? try try) is true (try try) should run run-forever forever.

if (try try) runs forever this implies (halts? try try) is true.
This contradicts that if (halts? try try) is false (try try) should return 'halted.

These are contradictions due to the bad assumption it is possible to write halts?

|#