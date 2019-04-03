#|

Exercise 3.20: Draw environment diagrams to illustrate the
evaluation of the sequence of expressions

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
 17 

using the procedural implementation of pairs given above.
(Compare Exercise 3.11.)

|#

#| Code from book |#
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "cons" "undefined operation" m))))
  dispatch)
