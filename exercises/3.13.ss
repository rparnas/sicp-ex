#|

Exercise 3.13: Consider the following "make-cycle"
procedure, which uses the "last-pair" procedure defined in
Exercise 3.12:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

Draw a box-and-pointer diagram that shows the structure "z"
created by

(define z (make-cycle (list 'a 'b 'c)))

What happens if we try to compute "(last-pair z)"?

|#

#| Answer 

[a][-]> [b][-]> [c][|]
 ^                  |
 |-------------------

You've created a cycle. If last-pair doesn't have cycle detection then z appears
as a list that goes on forver and last-pair will hang.

> (define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)
> (define z (make-cycle (list 'a 'b 'c)))
> z
Warning in pretty-print: cycle detected; proceeding with (print-graph #t)
#0=(a b c . #0#)
> (last-pair z)
Exception in last-pair: (a b c a b c ...) is circular
Type (debug) to enter the debugger.

|#
