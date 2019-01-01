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

