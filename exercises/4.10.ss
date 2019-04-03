#|

Exercise 4.10: By using data abstraction, we were able to
write an "eval" procedure that is independent of the
particular syntax of the language to be evaluated. To
illustrate this, design and implement a new syntax for
Scheme by modifying the procedures in this section, without
changing "eval" or "apply".

|#

(load-ex "4.9")

#| Answer

and tolerates "and" or "&&"

or tolerates "or" or "||"

|#

(define (is-and? exp) (or (tagged-list? exp 'and) (tagged-list? exp '&&)))
(define (is-or? exp) (or (tagged-list? exp 'or) (tagged-list? exp '||)))

#| Tests |#
(define-test (eval-one '(&&)) 'true)
(define-test (eval-one '(&& true)) 'true)
(define-test (eval-one '(&& true false)) 'false)
(define-test (eval-one '(&& false true)) 'false)

(define-test (eval-one '(||)) 'false)
(define-test (eval-one '(|| true)) 'true)
(define-test (eval-one '(|| true false)) 'true)
(define-test (eval-one '(|| false true)) 'true)

