#|

Exercise 4.18: Consider an alternative strategy for scanning
out definitions that translates the example in the text to

(lambda <vars>
  (let ((u '*unassigned*) (v '*unassigned*))
    (let ((a <e1>) (b <e2>))
      (set! u a)
      (set! v b))
    <e3>))

Here "a" and "b" are meant to represent new variable names,
created by the interpreter, that do not appear in the user's
program. Consider the "solve" procedure from Section 3.5.4:

(define (solve f y0 dt)
  (define  y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

Will this procedure work if internal definitions are scanned
out as shown in this exercise? What if they are scanned out
as shown in the text? Explain.

|#

#| Answer 

solve will be scanned out to

(lambda (f y0 dt)
  (let ([y '*unassigned*] 
        [dy '*unassigned*])
    (let ([a (integral (delay dy) y0 dt)]
          [b (stream-map f y)])
      (set! y a)
      (set! dy b)
      y)))

This won't work because at the time b's value is evaluated, y has a value of
'*unassigned* and this value is passed to stream-map and incorporated into the
resultant stream.

If they are scanned out as shown earlier in the text

(lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
  (set! y (integral (delay dy) y0 dt))
  (set! dy (stream-map f y))
  y))

This will work because (stream-map f y) will pass the final value of y to
stream-map, and we happen to know that integral won't force (delay dy) until
after the stream y is returned, thus it will access the final value of dy.

|#


