#|

Exercise 3.34: Louis Reasoner wants to build a squarer, a
constraint device with two terminals such that the value of
connector "b" on the second terminal will always be the
square of the value "a" on the first terminal. He proposes
the following simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

There is a serious flaw in this idea. Explain.

|#

#| Answer 

As in the test below, the multiplier doesn't check to see if its m1 and m2 are
the same thus it doesn't know to set m1 and m2 when just p is set.

|#

(load-ex "3.33")

(define (bad-squarer a b)
  (multiplier a a b))

#| Tests |#

(define-test
  (let ([a (make-connector)]
        [b (make-connector)])
    (bad-squarer a b)
    (probe "a" a)
    (probe "b" b)
    (set-value! b 4 'user)
    (flush-outs))
  '("b = 4"))
