#|

Exercise 3.73: We can model electrical circuits using
streams to represent the values of currents or voltages at a
sequence of times. For instance, suppose we have an RC
circuit consisting of a resistor of resistance R and a
capacitor of capacitance C in series. The voltage response v
of the circuit to an injected current i is determined by the
formula in Figure 3.33, whose structure is shown by the
accompanying signal-flow diagram.

Figure 3.33: An RC circuit and the associated signal-flow
diagram.

  +        v        -

 ->----'\/\/\,---| |---
  i       R         C


                  / t
               1  |
 v  =  v   +  --- |  i dt  +  R i
        0      C  |
                  / 0

         +--------------+
     +-->|   scale: R   |---------------------+   |\_
     |   +--------------+                     |   |  \_
     |                                        +-->|    \   v
  i  |   +--------------+     +------------+      | add >--->
 ----+-->|  scale: 1/C  |---->|  integral  |----->|   _/
         +--------------+     +------------+      | _/
                                    |             |/
				   v
				    0

Figure 3.33: An RC circuit and the associated signal-flow
diagram.

Write a procedure "RC" that models this circuit. "RC" should
take as inputs the values of R, C, and dt and should return
a procedure that takes as inputs a stream representing the
current i and an initial value for the capacitor voltage v_0
and produces as output the stream of voltages v. For
example, you should be able to use "RC" to model an RC
circuit with R = 5 ohms, C = 1 farad, and a 0.5-second time
step by evaluating "(define RC1 (RC 5 1 0.5))". This defines
"RC1" as a procedure that takes a stream representing the
time sequence of currents and an initial capacitor voltage
and produces the output stream of voltages.

|#

(load-ex "3.72")

#| Code from book |#
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

#| Answers |#

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1.0 C)) v0 dt))))

#| Tests |#

;;; circuit with R= 5 ohms, C = 1 farad, 0.5 second time step
(define RC1 (RC 5 1 0.5))

(define-test (map (lambda (i) (stream-ref (RC1 ones 0) i)) (iota 5))
             '(5 5.5 6.0 6.5 7.0))

(define-test (map (lambda (i) (stream-ref (RC1 integers 2) i)) (iota 5))
             '(7 12.5 18.5 25.0 32.0))

#| Notes 

I don't understand what the v equation means. You input v0 and yet the output
stream at 0 does not equal v0.

|#
