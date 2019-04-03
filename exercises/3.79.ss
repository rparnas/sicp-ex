#|

Exercise 3.79: Generalize the "solve-2nd" procedure of
Exercise 3.78 so that it can be used to solve general
second-order differential equations d^2y / dt^2 = f(dy / dt,
y).

|#

(load-ex "3.78")

#| Answer |#

(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

#| Tests 

Using wolfram alpha:
  f''(t) - 5f'(t) - 4f(t) = 0, f(0)=1, f'(0)=1
  f(t) = 1/82 e^(-1/2 (sqrt(41) - 5) t) ((41 - 3 sqrt(41)) e^(sqrt(41) t) + 41 + 3 sqrt(41))
  f(0) = 1
  f(1) = 79.909
  f(1.5) = 1376.49

dt=0.0001
  > (define dt 0.0001)
  > (stream-ref (solve-2nd (lambda (dy y) (+ (* 5 dy) (* 4 y))) 1 1 dt) 0)
  1
  > (stream-ref (solve-2nd (lambda (dy y) (+ (* 5 dy) (* 4 y))) 1 1 dt) (/ 1 dt))
  79.77990005876653
  > (stream-ref (solve-2nd (lambda (dy y) (+ (* 5 dy) (* 4 y))) 1 1 dt) (/ 1.5 dt))
  1373.1373034830951
  >

  Same as 3.78.

|#