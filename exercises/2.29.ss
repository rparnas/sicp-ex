#|

Exercise 2.29: A binary mobile consists of two branches, a
left branch and a right branch. Each branch is a rod of a
certain length, from which hangs either a weight or another
binary mobile. We can represent a binary mobile using
compound data by constructing it from two branches (for
example, using "list"):

(define (make-mobile left right)
  (list left right))

A branch is constructed from a "length" (which must be a
number) together with a "structure", which may be either a
number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

a. Write the corresponding selectors "left-branch" and
"right-branch", which return the branches of a mobile, and
"branch-length" and "branch-structure", which return the
components of a branch.

b. Using your selectors, define a procedure "total-weight"
that returns the total weight of a mobile.

c. A mobile is said to be balanced if the torque applied by
its top-left branch is equal to that applied by its
top-right branch (that is, if the length of the left rod
multiplied by the weight hanging from that rod is equal to
the corresponding product for the right side) and if each of
the submobiles hanging off its branches is balanced. Design
a predicate that tests whether a binary mobile is balanced.

d. Suppose we change the representation of mobiles so that
the constructors are

(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))

How much do you need to change your programs to convert to
the new representation?

|#

#| Code from book |#
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure)) 

#| Answer |#
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (is-leaf-branch? branch)
  (not (pair? (branch-structure branch))))

(define (branch-weight branch)
  ((if (is-leaf-branch? branch) 
       (lambda (x) x) 
       total-weight) (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (define (branch-balanced? branch)
    (or (is-leaf-branch? branch) (balanced? (branch-structure branch))))
  (let ([left (left-branch mobile)]
        [right (right-branch mobile)])
    (and (= (branch-torque left) (branch-torque right))
         (branch-balanced? left)
         (branch-balanced? right))))

#| New Representation 
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-structure branch)
  (cdr branch))
|#

#| Tests |#
(define m1 (make-mobile (make-branch 4 6) (make-branch 5 (make-mobile (make-branch 3 7) (make-branch 9 8)))))
(define m2 (make-mobile (make-branch 4 6) (make-branch 2 (make-mobile (make-branch 5 8) (make-branch 10 4)))))
(define-test (total-weight m1) 21)
(define-test (total-weight m2) 18)
(define-test (balanced? m1) #f)
(define-test (balanced? m2) #t)
