#|

Exercise 2.63: Each of the following two procedures converts
a binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                      (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                              (right-branch tree)
                              result-list)))))
  (copy-to-list tree '()))

a. Do the two procedures produce the same result for every
tree? If not, how do the results differ? What lists do the
two procedures produce for the trees in Figure 2.16?

b. Do the two procedures have the same order of growth in
the number of steps required to convert a balanced tree with
n elements to a list? If not, which one grows more slowly?

|#

(define false #f)
(define true #t)

#| Code from book |#
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry-set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

#| Code from exercises |#
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                      (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                              (right-branch tree)
                              result-list)))))
  (copy-to-list tree '()))

#| Example trees |#
(define (make-leaf entry)
  (make-tree entry '() '()))

(define tree0
  (make-tree 7
             (make-tree 3 (make-leaf 1) (make-leaf 5))
             (make-tree 9 '() (make-leaf 11))))

(define tree1
  (make-tree 3
      (make-leaf 1)
      (make-tree 7
   (make-leaf 5)
   (make-tree 9
       '()
       (make-leaf 11)))))

(define tree2
  (make-tree 5
      (make-tree 3
   (make-leaf 1)
   '())
      (make-tree 9
   (make-leaf 7)
   (make-leaf 11))))

#| Answer 

a. The procedures produce the same result for every tree.

b. Each tree->list-1 call makes a recursive call to each
branch and appends all the resuls from both recursive calls.
At the top level this means n copies and two recursive calls
each of which does n/2 coppies and two recursive calls and
so on. The depth of of the stack reaches Theta(log(n))
meaning Theta(n * log(n)) copies are made.

Each tree->list-2 call makes a tail-recrusive call to on the
left branch. This call as one of its arguments makes a
recursive call on the right-branch. Answer are consed up
from the right-branch. This means the depth of the stack is
still reaches Theta(log(n)) though in practice it is halved
vs tree->list-2 on a balanced tree. Theta(n) copies are
performed.

|#

#| Tests |#
(define-test (tree->list-1 tree0) '(1 3 5 7 9 11))
(define-test (tree->list-2 tree0) '(1 3 5 7 9 11))
(define-test (tree->list-1 tree1) '(1 3 5 7 9 11))
(define-test (tree->list-2 tree1) '(1 3 5 7 9 11))
(define-test (tree->list-1 tree2) '(1 3 5 7 9 11))
(define-test (tree->list-2 tree2) '(1 3 5 7 9 11))
