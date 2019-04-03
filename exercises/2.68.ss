#|

Exercise 2.68: The "encode" procedure takes as arguments a
message and a tree and produces the list of bits that gives
the encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

"encode-symbol" is a procedure, which you must write, that
returns the list of bits that encodes a given symbol
according to a given tree. You should design "encode-symbol"
so that it signals an error if the symbol is not in the tree
at all. Test your procedure by encoding the result you
obtained in Exercise 2.67 with the sample tree and seeing
whether it is the same as the original sample message.

|#

#| Code from book |#
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

#| Answer |#
(load-ex "2.67")

(define (encode-symbol sym tree)
  (define (in-tree? sym tree)
    (memq sym (symbols tree)))
  (define (iter bits tree)
    (if (leaf? tree)
        bits
        (let* ([left (left-branch tree)]
               [right (right-branch tree)]
               [zero (in-tree? sym left)]
               [one (in-tree? sym right)])
          (cond [(and (not zero) (not one))
                 (error 'encode-symbol "unknown symbol" sym)]
                [zero
                 (iter (cons 0 bits) left)]
                [else
                 (iter (cons 1 bits) right)]))))
  ;; (set! iter (tracize iter))
  (reverse (iter '() tree)))

#| Tests |#
(define-test (encode (decode sample-message sample-tree) sample-tree) 
             '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define-test sample-message 
             '(0 1 1 0 0 1 0 1 0 1 1 1 0))
