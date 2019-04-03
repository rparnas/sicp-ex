#|

Exercise 3.30: Figure 3.27 shows a ripple-carry adder formed
by stringing together n full-adders. This is the simplest
form of parallel adder for adding two n-bit binary numbers.
The inputs A_1, A_2, A_3,..., A_n and B_1, B_2, B_3,..., B_n
are the two binary numbers to be added (each A_k and B_k is
a 0 or a 1). The circuit generates S_1, S_2, S_3,..., S_n,
the n bits of the sum, and C, the carry from the addition.
Write a procedure "ripple-carry-adder" that generates this
circuit. The procedure should take as arguments three lists
of n wires each---the A_k, the B_k, and the S_k---and also
another wire C. The major drawback of the ripple-carry adder
is the need to wait for the carry signals to propagate. What
is the delay needed to obtain the complete output from an
n-bit ripple-carry adder, expressed in terms of the delays
for and-gates, or-gates, and inverters?

|#

#| Answer 

==Propagation Delay Analysis==
and-delay is a, or-delay is o, invesion-delay is i

half-adder A to S: a + [max of (o) OR (a + i)]
half-adder A to C: a
half-adder B to S: a + [max of (o) OR (a + i)]
half-adder B to C: a
full-adder A to SUM: [half-adder A to S]
                     a + [max of (o) OR (a + i)]
full-adder A to Cout: [half-adder A to C] + o
                      a + o
full-adder B to SUM: [half-adder A to S] + [half-adder B to C] + o
                      2a + o + [max of (o) OR (a + i)]
full-adder Cin to SUM: [half-adder B to C] + [half-adder B to S]
                       2a + [max of (o) OR (a + i)]             
full-adder Cin to Cout: [half-adder B to C] + [half-adder B to C] + o
                        2a + o

In the ripple-carry-adder, all A and B signals arrive immediately. Cin arrives
at the first adder immediately and travels through n-1 adders along the path
Cin to Count which takes (n-1)*(2a + o) = (2n-2)a + (n-1)o. The final Cout
becomes correct after one additional Cin to Cout path which is 2a + o

The final SUM becomes correct after one additional Cin to SUM path, which is 2a
+ [max of (o) OR (a + i)].

Final Cout is correct: 
  [(2n-2)a + (n-1)o] + 2a + o
  2na + no

Time until final SUM is correct: 
  [(2n-2)a + (n-1)o] + 2a + [max of (o) OR (a + i)]  
  2na + (n-1)o + [max of (o) or (a + i)]
  max of (2na + no) OR ((2n+1)a + (n-1)o + i)

|#

(define (ripple-carry-adder a b s c)
  (if (not (null? a))
    (let ([c-out (make-wire)])
      (full-adder (car a) (car b) c-in (car s) c-out)
      (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-out))
    'ok))

#| Tests -- not runnable |#
