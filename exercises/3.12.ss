#|

Exercise 3.12: The following procedure for appending lists
was introduced in Section 2.2.1:

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

"append" forms a new list by successively "cons"ing the
elements of "x" onto "y". The procedure "append!" is similar
to "append", but it is a mutator rather than a constructor.
It appends the lists by splicing them together, modifying
the final pair of "x" so that its "cdr" is now "y". (It is
an error to call "append!" with an empty "x".)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

Here "last-pair" is a procedure that returns the last pair
in its argument:

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

Consider the interaction

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
 (a b c d) 
(cdr x)
<response>
(define w (append! x y))
w
 (a b c d) 
(cdr x)
<response>

What are the missing <response>s? Draw box-and-pointer
diagrams to explain your answer.

|#

#| Answer 

State 1:
  x: [a][-]> [b][\]
  y: [c][-]> [d][\]
  z: [a][-]> [b][-]> y

State 2:
  x: [a][-]> [b][-]> y
  y: [c][-]> [d][\]
  z: [a][-]> [b][-]> y
  w: x

> (define x (list 'a 'b))
> (define y (list 'c 'd))
> (define z (append x y))
> z
(a b c d) 
; at State 1: (cdr x) points to pair ('b . null) aka proper list (b).
> (cdr x)
(b)
> (define w (append! x y))
> w
> (a b c d) 
; at State 2: (cdr x) points to pair ('b . y) aka a proper starting with b and
; continuing on to the contents of y: (b c d)
> (cdr x)
(b c d)

|#
