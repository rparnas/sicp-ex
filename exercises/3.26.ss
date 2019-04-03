#|

Exercise 3.26: To search a table as implemented above, one
needs to scan through the list of records. This is basically
the unordered list representation of Section 2.3.3. For
large tables, it may be more efficient to structure the
table in a different manner. Describe a table implementation
where the (key, value) records are organized using a binary
tree, assuming that keys can be ordered in some way (e.g.,
numerically or alphabetically). (Compare Exercise 2.66 of
Chapter 2.)

|#

#| Answer |#

;;; 2.63:
;;;   entry, left-branch, right-branch, make-tree -- Theta(1)
;;;   tree->list-2 -- Theta(n)
;;; 2.64:
;;;   list->tree -- Theta(n)
(load-ex "2.63") 
(load-ex "2.64")
(no-regression)

;;; adapted from 2.62 union-set -- Theta(n)
(define (union-alist set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let* ([x1 (car set1)]
                     [x2 (car set2)]
                     [k1 (entry-key x1)]
                     [k2 (entry-key x2)])
                (cond [(= k1 k2)
                       (cons x1 (union-alist (cdr set1) (cdr set2)))]
                      [(< k1 k2)
                       (cons x1 (union-alist (cdr set1) set2))]
                      [(< k2 k1)
                       (cons x2 (union-alist set1 (cdr set2)))]))]))

;;; adapted from 2.63 element-of-set? -- Theta(log(n))
(define (assoc-atree key atree)
  (if (null? atree)
      #f
      (let* ([e (entry atree)]
             [k (entry-key e)])
        (cond [(= key k) e]
              [(< key k) (assoc-atree key (left-branch atree))]
              [(> key k) (assoc-atree key (right-branch atree))]))))

;;; adapted from 2.65 union-set-tree -- Theta(n)
(define (union-set-atree set0 set1)
  (list->tree (union-alist (tree->list-2 set0)
                           (tree->list-2 set1))))

;;; new 
(define (make-entry key value) (cons key value))
(define (entry-key entry) (car entry))
(define (entry-value entry) (cdr entry))
(define (set-entry-value! entry value) (set-cdr! entry value))

;;; new
(define (make-table)
  (let ([table '()])
    (define (lookup key)
      (let ([entry (assoc-atree key table)])
        (if entry
            (entry-value entry)
            #f)))
    (define (insert! key value)
      (let ([entry (assoc-atree key table)])
        (if entry
            (set-entry-value! entry value)
            (set! table 
                  (union-set-atree table
                                   (make-tree (make-entry key value) '() '()))))))
    (define (dispatch m)
      (cond [(eq? m 'lookup) lookup]
            [(eq? m 'insert!) insert!]
            [(eq? m 'table) table]
            [else (error "table" "Unknown operation" m)]))
    dispatch))

(define (lookup key table) ((table 'lookup) key))
(define (insert! key value table) ((table 'insert!) key value))
(define (table table) (table 'table))

#| Tests -- infrastructure |#
(define-test (let* ([a '((1 a) (2 b) (3 c))]
                    [t (list->tree a)])
              (list (assoc-atree 1 t)
                    (assoc-atree 2 t)
                    (assoc-atree 3 t)
                    (assoc-atree 4 t)))
              '((1 a)
                (2 b)
                (3 c)
                #f))

#| Tests |#
(define-test (let ([t (make-table)])
               (begin
                (insert! 1 'a t)
                (insert! 2 'b t)
                (insert! 3 'c t)
                (insert! 4 'd t)
                (insert! 5 'e t)
                (insert! 6 'f t)
                (insert! 7 'g t)
                (table t)))
               '((4 . d) 
                  ((2 . b) 
                    ((1 . a) () ()) 
                    ((3 . c) () ()))
                  ((6 . f) 
                    ((5 . e) () ()) 
                    ((7 . g) () ()))))

(define-test (let ([t (make-table)])
               (define t (make-table))
               (insert! 1 'a t)
               (insert! 2 'a t)
               (insert! 3 'a t)
               (insert! 3 'b t)
               (lookup 3 t))
              'b)

#| Notes

if you were to allow an arbitrary number of keys, you could consider any
entry with n keys to be greater than those with n-1 keys, and then compare
two entries with the smame number of keys with the first key considered
the most signficant.

|#
