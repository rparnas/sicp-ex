#|

Exercise 4.11: Instead of representing a frame as a pair of
lists, we can represent a frame as a list of bindings, where
each binding is a name-value pair. Rewrite the environment
operations to use this alternative representation.

|#

(load-ex "4.10")

#| Answer -- frames |#

(define (make-frame variables values) (cons 'frame (map cons variables values)))
(define (frame-variables frame) (error "frame-variables" "deprecated"))
(define (frame-values frame) (error "frame-values" "deprecated"))
(define (frame-has-binding? frame var) (if (assq var (cdr frame)) #t #f))
(define (frame-get-value frame var) (cdr (assq var (cdr frame))))
(define (frame-set!-binding frame var val) (set-cdr! (assq var (cdr frame)) val))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

#| Answer -- environment |#
(define (set-variable-value! var val env)
  (define (iter env)
    (if (eq? env the-empty-environment)
        (error "set!" "unbound variable" var)
        (let ([frame (first-frame env)])
          (if (frame-has-binding? frame var)
              (frame-set!-binding frame var val)
              (iter (enclosing-environment env))))))
  (iter env))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (if (frame-has-binding? frame var)
        (frame-set!-binding frame var val)
        (add-binding-to-frame! var val frame))))

(define (lookup-variable-value var env)
  (define (iter env)
    (if (eq? env the-empty-environment)
        (error "" "unbound-variable" var)
        (let ([frame (first-frame env)])
          (if (frame-has-binding? frame var)
              (frame-get-value frame var)
              (iter (enclosing-environment env))))))
  (iter env))

#| Tests -- regression tests are adequate |#

#| Notes

In the original implementation, the internal workings of frame were leaked into
what should be enviornment methods: set-variable-value! and define-variable!
That has been corrected here.

|#