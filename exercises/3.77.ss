#|

Exercise 3.77: The "integral" procedure used above was
analogous to the "implicit" definition of the infinite
stream of integers in Section 3.5.2. Alternatively, we can
give a definition of "integral" that is more like
"integers-starting-from" (also in Section 3.5.2):

(define (integral integrand initial-value dt)
  (cons-stream
   initial-value
   (if (stream-null? integrand)
       the-empty-stream
       (integral (stream-cdr integrand)
                 (+ (* dt (stream-car integrand))
                    initial-value)
                 dt))))

When used in systems with loops, this procedure has the same
problem as does our original version of "integral". Modify
the procedure so that it expects the "integrand" as a
delayed argument and hence can be used in the "solve"
procedure shown above.

|#

(load-ex "3.76")

#| Code from book |#

(define old-integral integral)

;;; modified to use old integral if the integrand is a stream.
(define (integral delayed-integrand initial-value dt)
  (define (is-stream? x)
    (and (pair? x)
         (not (procedure? (car x)))
         (procedure? (cdr x))))
  (define int
    (cons-stream
      initial-value
      (let ([integrand (force delayed-integrand)])
        (add-streams (scale-stream integrand dt) int))))
  (if (is-stream? delayed-integrand)
      (old-integral delayed-integrand initial-value dt)
      int))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

#| Answer |#
(define (integral-377 integrand initial-value dt)
  (cons-stream
   initial-value
   (let ([unpacked-integrand (force integrand)])
     (if (stream-null? unpacked-integrand)
         the-empty-stream
         (integral (delay (stream-cdr unpacked-integrand))
                   (+ (* dt (stream-car unpacked-integrand))
                      initial-value)
                   dt)))))

(define (solve-377 f y0 dt)
  (define y (integral-377 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

#| Tests 

(solve-377 f y0 dt)
  f -- what is dy/dt equal to in terms of y? / How does y relate to it derivative?
  y0 -- what is the value of y(t) at t=0?
  dt -- how accurate should the integration be?
  =====
  result -- stream representing y(t) -- (y(0) y(dt) y(2*dt)...

"A first-order differential equation connects a function y(t) to its derivative
dy/dt. The rate of change in y is decied by y itself (and possibly also by time
t)."

dy/dt = y -- linear differential equation. Solve for y.
dy/dt = y(t) = integral from 0..t y(s) ds + C

; > (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
2.716923932235896

; > (stream-ref (solve-377 (lambda (y) y) 1 0.001) 1000) ; f(1) = e^1 = 2.718...
2.716923932235896

; > (stream-ref (solve-377 (lambda (y) y) 1 0.001) 2000) ; f(2) = e^2 = 7.389...
7.381675653556165

|#
