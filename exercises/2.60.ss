#|

Exercise 2.60: We specified that a set would be represented
as a list with no duplicates. Now suppose we allow
duplicates. For instance, the set {1, 2, 3} could be
represented as the list "(2 3 2 1 3 2 2)". Design procedures
"element-of-set?", "adjoin-set", "union-set", and
"intersection-set" that operate on this representation. How
does the efficiency of each compare with the corresponding
procedure for the non-duplicate representation? Are there
applications for which you would use this representation in
preference to the non-duplicate one?

|#

#| Answer |#
(load-ex "2.59")
(no-regression)

; element-of-set -- no changes needed

; adjoin-set -- simplification
(define (adjoin-set x set)
  (cons x set))

; intersection-set -- no changes needed although a bit odd
; since depending on the order of the arguments the result
; may change but this is okay becuase different values may
; represent the same set.

; union-set -- simplification
(define (union-set set1 set2)
  (append set1 set2))

#|

element-of-set? is up to Theta(n) processed elements.

adjoin-set eliminated the element-of-set? call and thus went
from possibly Theta(n) to Theta(1) processed elements.

intersection-set is up to Theta(n^2) processed elements as
each element in set1 may be checked against each element in
set2.

union-set was up Theta(n^2) processed elements and Theta(n)
copies as each element in set1 could be checked against a
set2 and copied to a new list -- and for set2 each element
could be copied to a new list. Now this has improved to just
Theta(n) copies as we no longer check the sets against each
other.

Conclusion: The new implementation is simplier. You might
know that duplicates are rare and aren't worth constantly
weeding out. The more duplicates there are the more memory
will be used and Theta(n) operations could end up worse than
the equivalent Theta(n^2) operations under a no-duplicates
regime. It might be a good idea to create a seperate remove-
duplicates function for fine grain control. Or maybe
evaluate whether the linked list is the datastructure you
want.

|#

#| Tests |#
(define-test (union-set '(1 2 3) '(3 4 5)) '(1 2 3 3 4 5))