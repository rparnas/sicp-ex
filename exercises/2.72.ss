#|

Exercise 2.72: Consider the encoding procedure that you
designed in Exercise 2.68. What is the order of growth in
the number of steps needed to encode a symbol? Be sure to
include the number of steps needed to search the symbol list
at each node encountered. To answer this question in general
is difficult. Consider the special case where the relative
frequencies of the n symbols are as described in Exercise
2.71, and give the order of growth (as a function of n) of
the number of steps needed to encode the most frequent and
least frequent symbols in the alphabet.

|#

#| Answer 

In the worst case of the worst tree (a completely unbalanced
tree that is basically a list) and the worst letter (the
leaf at the very bottom of the tree) there will be Theta(n)
calls to the iteration function and Theta(1) comparisons.

With the worst tree and the best symbol there will be
Theta(n) comparisons and Theta(1) iterations.

In the middle case there will be about ceil(n/2)=3
iterations and ceil(n/2)=3 comparisons each iteration so
perhaps this is an Theta(n^2) algorithm?

|#

#| Tests |#
(load-ex "2.69")

(define (make-worst-case-alphabet n)
  (define (iter result n)
    (if (= n 0)
        result
        (iter (cons (list n (expt 2 (- n 1))) result) (- n 1))))
  (reverse (iter '() n)))

(define (test sym n)
  (define (iter tree)
    (let* ([start (real-time)]
           [encoded (encode-symbol sym tree)]
           [stop (real-time)]
           [elapsed (- stop start)])
      elapsed))
  (let* ([alpha (make-worst-case-alphabet n)]
         [tree (generate-huffman-tree alpha)])
    (/ (+ (iter tree) 
          (iter tree) 
          (iter tree)) 3)))


(define worst-case-alphabet '((a 16) (b 8) (c 4) (d 2) (e 1)))
(define worst-case-tree (generate-huffman-tree worst-case-alphabet))

#| Tests

== Analysis of a single iteration==
First look at what functions are used in a single call to
the iteration function within encode-symbol.leaf?, left-
branch, right-branch, and symbols just use the car/cdr
family of functions, are trivially Theta(1) in all respects
we're looking for and are ignorable. The function in-tree?
uses memq against (symbols tree) aka the list of all
elements stored in a tree performing Theta(n) comparisons.

== Analysis of main body ==
reverse is used only once outside of iteration for Theta(n)
copies.

== Analysis of call stack ==
> (encode-symbol 'a worst-case-tree)
(#<procedure iter at 2.68.ss:1049> () (((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15) (leaf a 16) (e d c b a) 31))
(#<procedure iter at 2.68.ss:1049> (1) (leaf a 16))
(1)

> (encode-symbol 'b worst-case-tree)
(#<procedure iter at 2.68.ss:1049> () (((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15) (leaf a 16) (e d c b a) 31))
(#<procedure iter at 2.68.ss:1049> (0) ((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15))
(#<procedure iter at 2.68.ss:1049> (1 0) (leaf b 8))
(0 1)

> (encode-symbol 'c worst-case-tree)
(#<procedure iter at 2.68.ss:1049> () (((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15) (leaf a 16) (e d c b a) 31))
(#<procedure iter at 2.68.ss:1049> (0) ((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15))
(#<procedure iter at 2.68.ss:1049> (0 0) (((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7))
(#<procedure iter at 2.68.ss:1049> (1 0 0) (leaf c 4))
(0 0 1)

> (encode-symbol 'd worst-case-tree)
(#<procedure iter at 2.68.ss:1049> () (((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15) (leaf a 16) (e d c b a) 31))
(#<procedure iter at 2.68.ss:1049> (0) ((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15))
(#<procedure iter at 2.68.ss:1049> (0 0) (((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7))
(#<procedure iter at 2.68.ss:1049> (0 0 0) ((leaf e 1) (leaf d 2) (e d) 3))
(#<procedure iter at 2.68.ss:1049> (1 0 0 0) (leaf d 2))
(0 0 0 1)

> (encode-symbol 'e worst-case-tree)
(#<procedure iter at 2.68.ss:1049> () (((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15) (leaf a 16) (e d c b a) 31))
(#<procedure iter at 2.68.ss:1049> (0) ((((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7) (leaf b 8) (e d c b) 15))
(#<procedure iter at 2.68.ss:1049> (0 0) (((leaf e 1) (leaf d 2) (e d) 3) (leaf c 4) (e d c) 7))
(#<procedure iter at 2.68.ss:1049> (0 0 0) ((leaf e 1) (leaf d 2) (e d) 3))
(#<procedure iter at 2.68.ss:1049> (0 0 0 0) (leaf e 1))
(0 0 0 0)

  | iter | comparisons|
a | 2    | 5*1=5      |
b | 3    | 4*2=8      |
c | 4    | 3*3=9      |
d | 5    | 2*4=8      |
e | 5    | 1*5=5      |

For the best case letter the iteration will be called
once for the entire tree and immediately reach a leaf
with one additional call. This means Theta(1) calls
and Theta(n) symbol comparisons.

For the worst case letter the iteration function is invoked
n times. But at each level there is only 1 symbol comparison
because the symbols field of a tree has the least encoded
symbol first.

|#
