#|

Exercise 4.74: Alyssa P. Hacker proposes to use a simpler
version of "stream-flatmap" in "negate", "lisp-value", and
"find-assertions". She observes that the procedure that is
mapped over the frame stream in these cases always produces
either the empty stream or a singleton stream, so no
interleaving is needed when combining these streams.

a. Fill in the missing expressions in Alyssa's program.

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map <??>
              (stream-filter <??> stream)))

b. Does the query system's behavior change if we change it
in this way?

|#

(load-ex "4.69") ; skip 4.71, which was a hypothetical bad implementaiton

#| Answer

a. See code
b. The behavior is identical.

|#

;;; from 4.67
(define (negate operands frame-stream h)
  (simple-stream-flatmap
    (lambda (frame)
      (if (stream-null? (qeval (negated-query operands)
                               (singleton-stream frame)
                               h))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))
(put 'not 'qeval negate)

;;; from 4.67
(define (lisp-value call frame-stream h)
  (simple-stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate
             call
             frame
             (lambda (v f)
               (error "lisp-value" "unknown pat var" v))))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))
(put 'lisp-value 'qeval lisp-value)

;;; from 4.55
(define (find-assertions pattern frame)
  (simple-stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

;;; from prompt
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) (not (stream-null? s))) stream)))

#| Tests -- see regression |#