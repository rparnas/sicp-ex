#|

Exercise 4.12: The procedures "set-variable-value!",
"define-variable!" and "lookup-variable-value" can be
expressed in terms of more abstract procedures for
traversing the environment structure. Define abstractions
that capture the common patterns and redefine the three
procedures in terms of these abstractions.

|#

(load-ex "4.11")

#| Answer 

define-variable! needs no further improvement vs. 4.11, it already is well
abstracted from internal frame logic.

|#

(define (find-frame pred top-env)
  (define (iter env)
    (if (eq? env the-empty-environment)
        #f
        (let ([frame (first-frame env)])
          (if (pred frame)
              frame
              (iter (enclosing-environment env))))))
  (iter top-env))

(define (find-frame-with-binding var env)
  (find-frame (lambda (f) (frame-has-binding? f var)) env))

(define (set-variable-value! var val env)
  (let ([frame (find-frame-with-binding var env)])
    (if frame
        (frame-set!-binding frame var val)
        (error "" "unbound-variable" var))))

(define (lookup-variable-value var env)
  (let ([frame (find-frame-with-binding var env)])
    (if frame
        (frame-get-value frame var)
        (error "" "unbound-variable" var))))

#| Tests -- regression is adequate |#