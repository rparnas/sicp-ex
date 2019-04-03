#|

Exercise 3.24: In the table implementations above, the keys
are tested for equality using "equal?" (called by "assoc").
This is not always the appropriate test. For instance, we
might have a table with numeric keys in which we don't need
an exact match to the number we're looking up, but only a
number within some tolerance of it. Design a table
constructor "make-table" that takes as an argument a
"same-key?" procedure that will be used to test "equality"
of keys. "make-table" should return a "dispatch" procedure
that can be used to access appropriate "lookup" and
"insert!" procedures for a local table.

|#

#| Answer |#
(define (make-table same-key?)
  (let ([table (list '*table*)])
    (define (assp p alist)
      (if (null? alist)
        #f
        (let ([first (car alist)])
          (cond [(not (pair? first))
                 (error "assp" "improperly formed association list")]
                [(p (car first))
                 first]
                [else 
                 (assp p (cdr alist))]))))
    (define (lookup key table)
      (let ([record (assp (lambda (x) (same-key? key x)) 
                          (cdr table))])
        (if record
            (cdr record)
            #f)))
    (define (insert! key value table)
      (let ([record (assp (lambda (x) (same-key? key x)) 
                          (cdr table))])
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value)
                            (cdr table)))))
      (void))
    (define (dispatch m)
      (cond [(eq? m 'lookup) (lambda (key) (lookup key table))]
            [(eq? m 'insert!) (lambda (key value) (insert! key value table))]
            [else (error "table" "Unknown operation" m)]))
    dispatch))

(define (lookup key table) ((table 'lookup) key))
(define (insert! key value table) ((table 'insert!) key value))

#| Tests |#
(define t (make-table (lambda (a b) 
                        (or (and (even? a) (even? b))
                            (and (odd? a) (odd? b))))))
(insert! 1 'a t)
(insert! 2 'b t)
(insert! 3 'c t)
(insert! 4 'd t)

(define-test (list (lookup 1 t)
                   (lookup 2 t))
             (list 'c 
                   'd))