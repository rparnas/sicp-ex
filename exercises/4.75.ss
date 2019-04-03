#|

Exercise 4.75: Implement for the query language a new
special form called "unique". "unique" should succeed if
there is precisely one item in the data base satisfying a
specified query. For example,

(unique (job ?x (computer wizard)))

should print the one-item stream

(unique (job (Bitdiddle Ben) (computer wizard)))

since Ben is the only computer wizard, and

(unique (job ?x (computer programmer)))

should print the empty stream, since there is more than one
computer programmer. Moreover,

(and (job ?x ?j) (unique (job ?anyone ?j)))

should list all the jobs that are filled by only one person,
and the people who fill them.

There are two parts to implementing "unique". The first is
to write a procedure that handles this special form, and the
second is to make "qeval" dispatch to that procedure. The
second part is trivial, since "qeval" does its dispatching
in a data-directed way. If your procedure is called
"uniquely-asserted", all you need to do is

(put 'unique 'qeval uniquely-asserted)

and "qeval" will dispatch to this procedure for every query
whose "type" ("car") is the symbol "unique".

The real problem is to write the procedure
"uniquely-asserted". This should take as input the
"contents" ("cdr") of the "unique" query, together with a
stream of frames. For each frame in the stream, it should
use "qeval" to find the stream of all extensions to the
frame that satisfy the given query. Any stream that does not
have exactly one item in it should be eliminated. The
remaining streams should be passed back to be accumulated
into one big stream that is the result of the "unique"
query. This is similar to the implementation of the "not"
special form.

Test your implementation by forming a query that lists all
people who supervise precisely one person.

|#

(load-ex "4.74")

#| Answer |#

;;; returns true if the given stream has a length equal to n
(define (stream-length-equals? s n)
  (cond [(= 0 n) (stream-null? s)]
        [(stream-null? s) (= 0 n)]
        [else (stream-length-equals? (stream-cdr s) (- n 1))]))
(define (is-singleton-stream? s)
  (stream-length-equals? s 1))

;;; modified from 4.67 negate
(define (uniquely-asserted operands frame-stream h)
  (stream-flatmap
    (lambda (frame)
      (let ([extended-frames (qeval (car operands) (singleton-stream frame) h)])
        (if (is-singleton-stream? extended-frames)
            extended-frames
            the-empty-stream)))
    frame-stream))
(put 'unique 'qeval uniquely-asserted)

;;; test code updates
(define clear-all!-old clear-all!)
(set! clear-all! (lambda ()
  (clear-all!-old)
  (put 'unique 'qeval uniquely-asserted)))

#| Tests |#
(define-test (do-query test-db 
  '(unique (job ?x (computer wizard))))
  '((unique (job (Bitdiddle Ben) (computer wizard)))))

(define-test (do-query test-db
  '(unique (job ?x (computer programmer))))
  '())

(define-test (do-query test-db
  '(and (job ?x ?j) (unique (job ?anyone ?j))))
  '((and (job (Bitdiddle Ben) (computer wizard))
         (unique (job (Bitdiddle Ben) (computer wizard))))
    (and (job (Tweakit Lem E) (computer technician))
         (unique (job (Tweakit Lem E) (computer technician))))
    (and (job (Reasoner Louis) (computer programmer trainee))
         (unique (job (Reasoner Louis) (computer programmer trainee))))
    (and (job (Warbucks Oliver) (administration big wheel))
         (unique (job (Warbucks Oliver) (administration big wheel))))
    (and (job (Scrooge Eben) (accounting chief accountant))
         (unique (job (Scrooge Eben) (accounting chief accountant))))
    (and (job (Cratchet Robert) (accounting scrivener))
         (unique (job (Cratchet Robert) (accounting scrivener))))
    (and (job (Aull DeWitt) (administration secretary))
         (unique (job (Aull DeWitt) (administration secretary))))))

;;; all people who supervise precisely one person
(define-test (do-query test-db
  '(and (job ?p . ?any0)
        (unique (supervisor ?any1 ?p))))
  '((and (job (Hacker Alyssa P) (computer programmer))
         (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
    (and (job (Scrooge Eben) (accounting chief accountant))
         (unique (supervisor (Cratchet Robert) (Scrooge Eben))))))