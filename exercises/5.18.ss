#|

Exercise 5.18: Modify the "make-register" procedure of
Section 5.2.1 so that registers can be traced. Registers
should accept messages that turn tracing on and off. When a
register is traced, assigning a value to the register should
print the name of the register, the old contents of the
register, and the new contents being assigned. Extend the
interface to the machine model to permit you to turn tracing
on and off for designated machine registers.

|#

(load-ex "5.17")

#| Answer -- added to 5.15 |#

#| Tests 

> (define (m) (make-machine (list) '((assign a (const 1)) (assign b (const 2)) (assign c (const 3)))))
> (begin (trace-on-reg m 'b) (start m))
> (let ([m (m)]) (begin (trace-on-reg m 'b) (start m)))
b: *unassigned* -> 2

|#