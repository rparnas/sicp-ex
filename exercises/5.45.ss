#|

Exercise 5.45: By comparing the stack operations used by
compiled code to the stack operations used by the evaluator
for the same computation, we can determine the extent to
which the compiler optimizes use of the stack, both in speed
(reducing the total number of stack operations) and in space
(reducing the maximum stack depth). Comparing this optimized
stack use to the performance of a special-purpose machine
for the same computation gives some indication of the
quality of the compiler.

a. Exercise 5.27 asked you to determine, as a function of n,
the number of pushes and the maximum stack depth needed by
the evaluator to compute n! using the recursive factorial
procedure given above. Exercise 5.14 asked you to do the
same measurements for the special-purpose factorial machine
shown in Figure 5.11. Now perform the same analysis using
the compiled "factorial" procedure.

Take the ratio of the number of pushes in the compiled
version to the number of pushes in the interpreted version,
and do the same for the maximum stack depth. Since the
number of operations and the stack depth used to compute n!
are linear in n, these ratios should approach constants as n
becomes large. What are these constants? Similarly, find the
ratios of the stack usage in the special-purpose machine to
the usage in the interpreted version.

Compare the ratios for special-purpose versus interpreted
code to the ratios for compiled versus interpreted code. You
should find that the special-purpose machine does much
better than the compiled code, since the hand-tailored
controller code should be much better than what is produced
by our rudimentary general-purpose compiler.

b. Can you suggest improvements to the compiler that would
help it generate code that would come closer in performance
to the hand-tailored version?

|#

(load-ex "5.40")

#| Answer 

a. Stats for recursive factorial

  Dedicated Machine (5.14)
    * pushes: 2n - 2
    * max depth: 2n - 2

  Interpreter (explicit-control evaluator) (5.27)
    * pushes: 32n - 10
    * max depth: 5n + 3

  Compiled:
    * pushes: 2n - 2
    * max depth: 2n - 2

  The interpreter uses 16x pushes and 2.5x stack depth as the dedicated machine.
  The compiled version has the same stack efficiency as the dedicated machine.

  b. The compiled version matches the stack efficiency of the dedicated machine
  given the optimizations already added to the compiler, such as open-coded
  primitives. The only stack operations save env & continue before a recursive
  call is made. I don't think there are any simple non-esoteric ways to improve
  efficiency further. I'd also be concerned about the compiler's
  maintainability. However I put a few ideas for exploration below:

  (1) Use non-stack memory instead of the stack. But this is likely less
efficient as the idea behind stack memory is that it is managed simply by
incrementing and decrementing a single pointer.

  (2) A human can see that this code should be re-written as an iterative,
tail-call optimized algorithm. Could this thought process be integrated into the
compiler? Perhaps something like "If every possible return expression consists
of a combination of primitive operations and a single recursive call, transform
the function to use an inner iterative function?"

  (3) For each recursive call, n is mutated by a reversable operation. Could we
restore n by adding one after we pop out of the recursive call instead of
restoring the environment?

  (4) Could we do something with the face factorial is almost tail-recursive
except for multiply, which is a primitive commutative operation? For example,
instead of a target of val which causes an (assign (reg val) ...) could we use
a target which inclues an operation? Like "multiply your result into val" or
(assign (op *) (reg val) <result>)?

|#

#| Notes

Temporarily made compile-test in 5.40 return

(cons (get-register-contents m 'val) ((m 'stack) 'get-statistics))

to produce statistics.

> (map factorial-recur-compile-test (cdr (iota 10)))
 (("1!" 1 . "pushes=0, max-depth=0") 
  ("2!" 2 . "pushes=2, max-depth=2")
  ("3!" 6 . "pushes=4, max-depth=4")
  ("4!" 24 . "pushes=6, max-depth=6")
  ("5!" 120 . "pushes=8, max-depth=8")
  ("6!" 720 . "pushes=10, max-depth=10")
  ("7!" 5040 . "pushes=12, max-depth=12")
  ("8!" 40320 . "pushes=14, max-depth=14")
  ("9!" 362880 . "pushes=16, max-depth=16"))

|#

(define (factorial-recur-compile-test n)
  (define fact-exp
    `(begin 
      (define (factorial n)
        (if (= n 1)
            1 
            (* (factorial (- n 1)) n)))
      (factorial ,n)))
  (cons (format "~a!" n) (compile-test fact-exp)))

(define (perfect-factorial-recursive-machine)
  (make-machine 
    (list (list '= =) (list '- -) (list '* *))
    '((assign continue (label fact-done))     ; set up final return address
      fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        ;; Set up for the recursive call by saving n and continue.
        ;; Set up continue so that the computation will continue
        ;; at after-fact when the subroutine returns.
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
      after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
        (goto (reg continue))                   ; return to caller
      base-case
        (assign val (const 1))                  ; base case: 1! = 1
        (goto (reg continue))                   ; return to caller
      fact-done)))
