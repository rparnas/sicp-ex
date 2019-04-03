#|

Exercise 2.13: Show that under the assumption of small
percentage tolerances there is a simple formula for the
approximate percentage tolerance of the product of two
intervals in terms of the tolerances of the factors. You may
simplify the problem by assuming that all numbers are
positive.

After considerable work, Alyssa P. Hacker delivers her
finished system. Several years later, after she has
forgotten all about it, she gets a frenzied call from an
irate user, Lem E. Tweakit. It seems that Lem has noticed
that the formula for parallel resistors can be written in
two algebraically equivalent ways:

 R_1 R_2
---------
R_1 + R_2

and

      1
-------------
1/R_1 + 1/R_2

He has written the following two programs, each of which
computes the parallel-resistors formula differently:

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

Lem complains that Alyssa's program gives different answers
for the two ways of computing. This is a serious complaint.

|#

#| Answer

An interval in terms of center c and percent p is:

{ c-(c*p), c+(c*p) }
{ c*(1-p), c*(1+p) }

Given two positive intervals a and b, a*b is:

{ (ca*(1-pa)) * (cb*(1-pb)), (ca*(1+pa)) * (cb*(1+pb)) }
{ ca*cb*(1-pa)*(1-pb), ca*cb*(1+pa)*(1+pb) }
{ ca*cb*(1 - pa - pb + pa*pb), ca*cb*(1 + pa + pb - pa*pb }

The term pa*pb in the endpoints of the interval is going to
have little influence. You can assume it is near zero for
small values of pa and pb:

{ ca*cb*(1- pa - pb), ca*cb*(1 + pa + pb) }

So the product intervals percent is approximately pa + pb.

|#

