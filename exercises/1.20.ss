#|

Exercise 1.20: The process that a procedure generates is of
course dependent on the rules used by the interpreter. As an
example, consider the iterative "gcd" procedure given above.
Suppose we were to interpret this procedure using
normal-order evaluation, as discussed in Section 1.1.5. (The
normal-order-evaluation rule for "if" is described in
Exercise 1.5.) Using the substitution method (for normal
order), illustrate the process generated in evaluating "(gcd
206 40)" and indicate the "remainder" operations that are
actually performed. How many "remainder" operations are
actually performed in the normal-order evaluation of "(gcd
206 40)"? In the applicative-order evaluation?

|#

#| Answer

; applicative-order -- 4 calls
(gcd 206 40)
(gcd 40 (r 206 40)) ; 1
(gcd 40 6)
(gcd 6 (r 40 6))    ; 2
(gcd 6 4)
(gcd 4 (r 6 4))     ; 3
(gcd 4 2)
(gcd 2 (r 4 2))     ; 4
(gcd 2 0)
2

; normal-order -- 18 calls
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (r 206 40)))
(gcd 40 (r 206 40))
(if (= (r 206 40) 0) 40 (gcd (r 206 40) (r ...

|#