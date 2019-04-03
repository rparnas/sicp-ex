#|

Exercise 4.25: Suppose that (in ordinary applicative-order
Scheme) we define "unless" as shown above and then define
"factorial" in terms of "unless" as

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

What happens if we attempt to evaluate "(factorial 5)"? Will
our definitions work in a normal-order language?

|#

#| Answer 

In an applicative order language (factorial 5) will never terminate.

In a normal-order language it will terminate because the recursive call to
factorial will only be evaluated if needed.

|#