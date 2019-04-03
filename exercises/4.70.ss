#|

Exercise 4.70: What is the purpose of the "let" bindings in
the procedures "add-assertion!" and "add-rule!" ? What would
be wrong with the following implementation of
"add-assertion!" ? Hint: Recall the definition of the
infinite stream of ones in Section 3.5.2: "(define ones
(cons-stream 1 ones))".

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)

|#

#| Answer

;;; original from 4.55
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ([old-assertions THE-ASSERTIONS])
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

The stream's cdr is delayed. If you want the old value of the stream to be the
new stream's cdr, you must capture it.

The new stream will consider the stream cdr to be the new stream which will
result in an infinite stream of just the first value.

|#