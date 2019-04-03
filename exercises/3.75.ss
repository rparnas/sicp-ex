#|

Exercise 3.75: Unfortunately, Alyssa's zero-crossing
detector in Exercise 3.74 proves to be insufficient, because
the noisy signal from the sensor leads to spurious zero
crossings. Lem E. Tweakit, a hardware specialist, suggests
that Alyssa smooth the signal to filter out the noise before
extracting the zero crossings. Alyssa takes his advice and
decides to extract the zero crossings from the signal
constructed by averaging each value of the sense data with
the previous value. She explains the problem to her
assistant, Louis Reasoner, who attempts to implement the
idea, altering Alyssa's program as follows:

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-value)
     (make-zero-crossings
      (stream-cdr input-stream) avpt))))

This does not correctly implement Alyssa's plan. Find the
bug that Louis has installed and fix it without changing the
structure of the program. (Hint: You will need to increase
the number of arguments to "make-zero-crossings".)

|#

(load-ex "3.74")

#| Answer 

In the above, sign-change-detector compares i and i-1 by comparing
  * [average of i and i-1]
  * [i-1]
Instead it should be
  * [average of i and i-1]
  * [average of i-1 and i-2]

|#

(define (make-smoothed-zero-crossings input-stream one-back-value two-back-value)
  (let* ([this-value (stream-car input-stream)]
         [this-avg (/ (+ this-value one-back-value) 2)]
         [one-back-avg (/ (+ one-back-value two-back-value) 2)])
    (cons-stream
      (sign-change-detector this-avg one-back-avg)
      (make-smoothed-zero-crossings
        (stream-cdr input-stream) this-value one-back-value))))

(define smoothed-zero-crossings
  (make-smoothed-zero-crossings sense-data 0 0))

#| Tests 

signal, smoothed signal, and zero-crossings
(0.00 0.00 1.00 2.00 1.50 1.00 0.50 -0.1 -2    -3.0 -2   -0.5   0.2  3   4  )
(0.00 0.00 0.50 1.50 1.75 1.25 0.75 0.20 -1.05 -2.5 -2.5 -1.25 -0.15 1.6 3.5)
(   x    x    0    0    0    0    0    0    -1    0    0     0     0   1   0)

|#

(define-test (map (lambda (i) (stream-ref smoothed-zero-crossings i)) (iota 13))
             '(0 0 0 0 0  0 -1 0 0 0 0 1 0))

#| Notes

Smoothing and zero-crossings should be seperate abstracted concerns. Several of
the late exercises in Section 3 are much harder than necessary because of their
disregard of the lessons of Section 2.

Many students, had they not been presented with the bad version of this code,
would have produced something more architecturally sound in their first draft.
See 3.76.

|#
