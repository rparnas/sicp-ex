#|

Exercise 2.64: The following procedure "list->tree" converts
an ordered list to a balanced binary tree. The helper
procedure "partial-tree" takes as arguments an integer n and
list of at least n elements and constructs a balanced tree
containing the first n elements of the list. The result
returned by "partial-tree" is a pair (formed with "cons")
whose "car" is the constructed tree and whose "cdr" is the
list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

a. Write a short paragraph explaining as clearly as you can
how "partial-tree" works. Draw the tree produced by
"list->tree" for the list "(1 3 5 7 9 11)".

b. What is the order of growth in the number of steps
required by "list->tree" to convert a list of n elements?

|#

#| Code from book |#
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

#| Answer 

a. This makes a tree given an ordered list. The left branch
is a subtree with half the elements, excluding the middle
element which is taken as the entry of the tree, and the
right branch is a subtree with the remaining elements.

The unique thing about the algorithm is that it uses only
cdr and the counter variable 'n' to chop up the list. The
advantage is that when you pursue a left-branch you don't
have to create a new list whose values are a copy of the
begining of the list 'elements'. Instead you only pass a
variable 'n' meaning "only use the first n values".

b. Each call to partial-tree returns a single node with a
single element or an empty dead end. Each call makes two
recursive calls with half the remaining elements to make
left and right branches. There are n nodes and 2n dead ends
max in a perfectly balanced tree, and the order of the
number of calls to partial-tree is Theta(n).

|#

#| Tests |#
(load-ex "2.63")
(no-regression)

#|

> (define partial-tree (tracize partial-tree))

> (list->tree '(1 2 3 4))
(#<procedure partial-tree at 2.64.ss:1794> (1 2 3 4) 4) ; 2-node
(#<procedure partial-tree at 2.64.ss:1794> (1 2 3 4) 1) ; 1-node
(#<procedure partial-tree at 2.64.ss:1794> (1 2 3 4) 0) ; 1-node left dead-end
(#<procedure partial-tree at 2.64.ss:1794> (2 3 4) 0)   ; 1-node right dead-end
(#<procedure partial-tree at 2.64.ss:1794> (3 4) 2)     ; 3-node
(#<procedure partial-tree at 2.64.ss:1794> (3 4) 0)     ; 3-node left dead-end   
(#<procedure partial-tree at 2.64.ss:1794> (4) 1)       ; 4-node
(#<procedure partial-tree at 2.64.ss:1794> (4) 0)       ; 4-node left dead-end
(#<procedure partial-tree at 2.64.ss:1794> () 0)        ; 4-node right dead-end
(2 
  (1 () ()) 
  (3 () 
     (4 () ())))

> (list->tree '(1 2 3 5))
(#<procedure partial-tree at 2.64.ss:1794> (1 2 3 4 5) 5)
(#<procedure partial-tree at 2.64.ss:1794> (1 2 3 4 5) 2)
(#<procedure partial-tree at 2.64.ss:1794> (1 2 3 4 5) 0)
(#<procedure partial-tree at 2.64.ss:1794> (2 3 4 5) 1)
(#<procedure partial-tree at 2.64.ss:1794> (2 3 4 5) 0)
(#<procedure partial-tree at 2.64.ss:1794> (3 4 5) 0)
(#<procedure partial-tree at 2.64.ss:1794> (4 5) 2)
(#<procedure partial-tree at 2.64.ss:1794> (4 5) 0)
(#<procedure partial-tree at 2.64.ss:1794> (5) 1)
(#<procedure partial-tree at 2.64.ss:1794> (5) 0)
(#<procedure partial-tree at 2.64.ss:1794> () 0)
(3 (1 () (2 () ())) (4 () (5 () ())))

|#

#| Notes

(partial-tree '(1 2 3) 3)
left-size: 1
right-size: 1
left-result: (partial-tree '(1 2 3) 1) --> '((1 () ()) . (2 3))
left-tree: '(1 () ())
non-left-lements '(2 3)
this-entry: 2
right-result: (partial-tree '(3) 1) --> '((3 () ()) . ())
right-tree: '(3 () ())
remaining-elets: '()
--> '[(2 (1 () ()) (3 () ())) . ()]

(partial-tree '(1 2 3) 1)
left-size: 0
right-size: 0
left-result: (partial-tree '(1 2 3) 0) --> '(() 1 2 3)
left-tree: '()
non-left-elements: '(1 2 3)
this-entry: 1
right-result: (partial-tree '(2 3) 0) --> '(() 2 3)
right-tree: '()
remaining-elets: '(2 3)
--> '((1 () ()) . (2 3))

(partial-tree '(3) 1)
left-size 0
right-size 0
left-result: (partial-tree '(3) 0) --> '(() 3)
left-tree: '()
non-left-elements '(3)
this-entry: 3
right-result (partial-tree '() 0) --> '(())
right-tree '()
remaining-elets: '()
--> '((3 () ()) . ())

Tree for (1 3 5 6 9 11):
(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
             5
           /   \
          1     9
         / \   / \
            3 7  11

|#