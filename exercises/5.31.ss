#|

Exercise 5.31: In evaluating a procedure application, the
explicit-control evaluator always saves and restores the
"env" register around the evaluation of the operator, saves
and restores "env" around the evaluation of each operand
(except the final one), saves and restores "argl" around the
evaluation of each operand, and saves and restores "proc"
around the evaluation of the operand sequence. For each of
the following combinations, say which of these "save" and
"restore" operations are superfluous and thus could be
eliminated by the compiler's "preserving" mechanism:

(f 'x 'y)
((f) 'x 'y)
(f (g 'x) y)
(f (g 'x) 'y)

|#

#| Answer 

|               | env      | env               | argl         | proc        |
|  expression   | operator | non-last operands | each operand | operand seq |
| ------------- | -------- | ----------------- | ------------ | ----------- |
| (f 'x 'y)     |          |                   |              |             |
| ((f) 'x 'y)   | needed   |                   |              |             |
| (f (g 'x) y)  |          | needed            | needed       | needed      |
| (f (g 'x) 'y) |          |                   | needed       | needed      |

Tecnically for the last two, "argl for each operand" is only needed for the
first operand but not the second, but I'll leave as needed since the
explicit-control evaluator doesn't differentiate that.

|#
