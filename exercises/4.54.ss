#|

Exercise 4.54: If we had not realized that "require" could
be implemented as an ordinary procedure that uses "amb", to
be defined by the user as part of a nondeterministic
program, we would have had to implement it as a special
form. This would require syntax procedures

(define (require? exp)
  (tagged-list? exp 'require))
(define (require-predicate exp)
  (cadr exp))

and a new clause in the dispatch in "analyze"

((require? exp) (analyze-require exp))

as well the procedure "analyze-require" that handles
"require" expressions. Complete the following definition of
"analyze-require".

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if <??>
                   <??>
                   (succeed 'ok fail2)))
             fail))))

|#

(load-ex "4.48") ; skip 4.49 ~ 4.50 which break compatability

#| Code from book |#
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

(define analyze-448 analyze)
(set! analyze (lambda (exp)
  (cond [(require? exp) (analyze-require exp)]
        [else (analyze-448 exp)])))

#| Answer |#
(define (analyze-require exp)
  (let ([pproc (analyze (require-predicate exp))])
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

#| Tests -- see regression tests |#