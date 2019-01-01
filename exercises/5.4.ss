#|

Exercise 5.4: Specify register machines that implement each
of the following procedures. For each machine, write a
controller instruction sequence and draw a diagram showing
the data paths.

a. Recursive exponentiation:

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

b. Iterative exponentiation:

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))

|#

