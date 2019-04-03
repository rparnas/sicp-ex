#|

Exercise 3.80: A series RLC circuit consists of a resistor,
a capacitor, and an inductor connected in series, as shown
in Figure 3.36. If R, L, and C are the resistance,
inductance, and capacitance, then the relations between
voltage (v) and current (i) for the three components are
described by the equations

v_R = i_R R

         d i_L
v_L = L -------
          d t

         d v_C
i_C = C -------
          d t

and the circuit connections dictate the relations

i_R = i_L = -i_C

v_C = v_L + v_R

Combining these equations shows that the state of the
circuit (summarized by v_C, the voltage across the
capacitor, and i_L, the current in the inductor) is
described by the pair of differential equations

d v_C        i_L
-----  =  -  ---
 d t          C

d i_L      1           R
-----  =  --- v_C  -  --- i_L
 d t       L           L

The signal-flow diagram representing this system of
differential equations is shown in Figure 3.37.

|#

(load-ex "3.79")

#| Answer |#

;;; return (cons [voltage of capacitor] [current of inductor])
(define (RLC R L C dt)
  (lambda (vc0 iL0)
    (define vC (integral (delay dvC) vc0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams 
                  (scale-stream vC (/ 1 L))
                  (scale-stream iL (- (/ R L)))))
    (cons vC iL)))

#| Tests 

;;; R=1, C=0.2, L=1, dt=0.1, iL0=0, vc0=10
(define example ((RLC 1 1 0.2 0.1) 10 0 ))
(define example-vc (car example))
(define example-il (cdr example))

> (map (lambda (i) (stream-ref example-vc i)) (iota 6))
(10 10 9.5 8.55 7.220000000000001 5.5955)

> (map (lambda (i) (stream-ref example-il i)) (iota 6))
(0 1.0 1.9 2.66 3.249 3.6461)

Did not manually compute answers, looked up first 5 elements of the streams.

|#
