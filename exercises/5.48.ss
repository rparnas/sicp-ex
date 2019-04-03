#|

Exercise 5.48: The "compile-and-go" interface implemented in
this section is awkward, since the compiler can be called
only once (when the evaluator machine is started). Augment
the compiler-interpreter interface by providing a
"compile-and-run" primitive that can be called from within
the explicit-control evaluator as follows:

 ;;; EC-Eval input: 
(compile-and-run
 '(define (factorial n)
    (if (= n 1) 1 (* (factorial (- n 1)) n))))
 ;;; EC-Eval value: 
 ok 
 ;;; EC-Eval input: 
(factorial 5)
 ;;; EC-Eval value: 
 120 

|#

(load-ex "5.47")

#| Answer -- Some implemenation in 5.32 |#

#| Tests |#

(define (eval-one exp)
  (let ([m (eval-machine)])
    (set-register-contents! m 'exp exp)
    (set-register-contents! m 'env (setup-environment))
    (set-register-contents! m 'flag #f)
    (start m)
    (get-register-contents m 'val)))

(define-test 
  (eval-one '(begin 
               (compile (define (x) 5)) 
               (x)))
  5)

(define-test
  (eval-one '(begin
               (compile (define (x) 5))
               (define (y) (x))
               (compile (define (z) (y)))
               (z)))
  5)