#|

Exercise 3.25: Generalizing one- and two-dimensional tables,
show how to implement a table in which values are stored
under an arbitrary number of keys and different values may
be stored under different numbers of keys. The "lookup" and
"insert!" procedures should take as input a list of keys
used to access the table.

|#

#| Answer 

Store as an alist. Each key is paired with a list. The first element of that
list is the value associated with that key (assuming there are no further keys)
and the rest of the list is a nested alist with the same structure.

|#

(define (make-table)
  (let ([table (list '*table* #f)])
    (define (key table) (car table))
    (define (value table) (cadr table))
    (define (alist table) (cddr table))
    (define (set-value! table value) (set-car! (cdr table) value))
    (define (set-alist! table alist) (set-cdr! (cdr table) alist))
    (define (assoc key alist)
      (cond [(null? alist)
             #f]
            [(equal? key (caar alist))
             (car alist)]
            [else
             (assoc key (cdr alist))]))
    (define (lookup keys table)
      (cond [(not table)
             #f]
            [(null? keys)
             (value table)]
            [else 
             (lookup (cdr keys) (assoc (car keys) (alist table)))]))
    (define (insert! keys value table)
      (if (null? keys)
          (set-value! table value)
          (let ([subtable (assoc (car keys) (alist table))])
            (if subtable
                (insert! (cdr keys) value subtable)
                (let ([new-subtable (list (car keys) #f)])
                  (set-alist! table
                              (cons new-subtable (alist table)))
                  (insert! (cdr keys) value new-subtable)))))
      (void))
    (define (dispatch m)
      (cond [(eq? m 'lookup) (lambda (keys) (lookup keys table))]
            [(eq? m 'insert!) (lambda (keys value) (insert! keys value table))]
            [(eq? m 'data) table]
            [else (error "table" "Unknown operation" m)]))
    dispatch))

(define (lookup keys table) ((table 'lookup) keys))
(define (insert! keys value table) ((table 'insert!) keys value))

#| Tests |#
(define t (make-table))
(insert! '() 1 t)
(insert! '(a) 2 t)
(insert! '(b) 3 t)
(insert! '(a a) 4 t)
(insert! '(a b) 5 t)
(insert! '(b a) 6 t)
(insert! '(b b) 7 t)
(insert! '(a a a) 8 t)
(insert! '(a a b) 9 t)
(insert! '(a b a) 10 t)

(define-test (map (lambda (keys) (lookup keys t))
                  '(() (a) (b) (a a) (a b) (b a) (b b) (a a a) (a a b) (a b a)))
                  '(1 2 3 4 5 6 7 8 9 10))
