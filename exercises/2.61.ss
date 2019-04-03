#|

Exercise 2.61: Give an implementation of "adjoin-set" using
the ordered representation. By analogy with
"element-of-set?" show how to take advantage of the ordering
to produce a procedure that requires on the average about
half as many steps as with the unordered representation.

|#

#| Answer 

Assuming that values are evenly distributed on average you
will only have to search through half of the list to figure
out where an element should go. However on average you'll
have to cons up a completely new list that copies half of
the elements in the set (i.e. the elements that need to go
before x in the set).

|#

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(= x (car set)) set]
        [(< x (car set)) (cons x set)]
        [else (cons (car set) 
                    (adjoin-set x (cdr set)))]))


#| Test |#
(define-test (adjoin-set 4 '(1 2 3)) '(1 2 3 4))
(define-test (adjoin-set 0 '(1 2 3)) '(0 1 2 3))
(define-test (adjoin-set 2 '(1 2 3)) '(1 2 3))
