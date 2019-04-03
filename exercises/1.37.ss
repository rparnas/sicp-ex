#|

Exercise 1.37:

a. An infinite continued fraction is an expression of the
form

           N_1
f = ---------------------
               N_2
    D_1 + ---------------
                   N_3
          D_2 + ---------
                D_3 + ...

As an example, one can show that the infinite continued
fraction expansion with the N_i and the D_i all equal to 1
produces 1 / Phi, where Phi is the golden ratio (described
in Section 1.2.2). One way to approximate an infinite
continued fraction is to truncate the expansion after a
given number of terms. Such a truncation---a so-called
k-term finite continued fraction---has the form

       N_1
-----------------
          N_2
D_1 + -----------
   ...   N_K
          + -----
             D_K

Suppose that "n" and "d" are procedures of one argument (the
term index i) that return the N_i and D_i of the terms of
the continued fraction. Define a procedure "cont-frac" such
that evaluating "(cont-frac n d k)" computes the value of
the k-term finite continued fraction. Check your procedure
by approximating 1 / Phi using

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

for successive values of "k". How large must you make "k" in
order to get an approximation that is accurate to 4 decimal
places?

b. If your "cont-frac" procedure generates a recursive
process, write one that generates an iterative process. If
it generates an iterative process, write one that generates
a recursive process.

|#

#| Answer |#

(define (count-frac n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1)  (/ (n k) (+ (d k) result)))))
  (iter k 0))

(define (count-frac-r n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 0))

#| Tests

;; The 11-th k-term approximates 1/phi to 4 places
> (count-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
0.6180555555555556

|#