#|

Exercise 1.34: Suppose we define the procedure

(define (f g) (g 2))

Then we have

(f square)
 4 
(f (lambda (z) (* z (+ z 1))))
 6 

What happens if we (perversely) ask the interpreter to
evaluate the combination "(f f)"? Explain.

|#

#| Answer

An error will occur because ventually it will try to apply 2
as a procedure.

(f f)
(f 2)
(2 2)

|#