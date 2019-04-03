#|

Exercise 2.66: Implement the "lookup" procedure for the case
where the set of records is structured as a binary tree,
ordered by the numerical values of the keys.

|#

#| Infrastructure to support tree of keyed values|#
#|(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))|#

(define (make-entry key data) (cons key data))
(define (key entry) (car entry))
(define (data entry) (cdr entry))

#| Rewrites to support arbitrary trees |#


#| Answer |#
(load-ex "2.65")

(define (lookup given-key tree)
  (if (null? tree)
      #f
      (let* ([e (entry tree)]
             [k (key e)])
        (cond [(equal? given-key k) 
               e]
              [(< given-key k) 
               (lookup given-key (left-branch tree))]
              [(> given-key k) 
               (lookup given-key (right-branch tree))]))))

#| Tests |#
(define t0 (list->tree '((1 . a) 
                         (2 . b)
                         (3 . c)
                         (4 . d))))
(define-test (lookup 3 t0) '(3 . c))
(define-test (lookup 4 t0) '(4 . d))
(define-test (lookup 7 t0) #f)
