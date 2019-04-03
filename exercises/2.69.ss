#|

Exercise 2.69: The following procedure takes as its argument
a list of symbol-frequency pairs (where no symbol appears in
more than one pair) and generates a Huffman encoding tree
according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

"make-leaf-set" is the procedure given above that transforms
the list of pairs into an ordered set of leaves.
"successive-merge" is the procedure you must write, using
"make-code-tree" to successively merge the smallest-weight
elements of the set until there is only one element left,
which is the desired Huffman tree. (This procedure is
slightly tricky, but not really complicated. If you find
yourself designing a complex procedure, then you are almost
certainly doing something wrong. You can take significant
advantage of the fact that we are using an ordered set
representation.)

|#

#| Code from book |#
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

#| Answer |#
(load-ex "2.68")

(define (successive-merge items)
  (cond [(null? items) items]
        [(null? (cdr items)) (car items)]
        [else (let ([smallest (car items)]
                    [second-smallest (cadr items)]
                    [rest (cddr items)])
                (successive-merge (adjoin-set (make-code-tree smallest second-smallest) rest)))]))

#| Tests |#
(define-test (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
             '((leaf A 4)
               ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
               (A B D C)
               8))
(define-test sample-tree
             '((leaf A 4)
               ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
               (A B D C)
               8))