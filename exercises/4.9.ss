#|

Exercise 4.9: Many languages support a variety of iteration
constructs, such as "do", "for", "while", and "until". In
Scheme, iterative processes can be expressed in terms of
ordinary procedure calls, so special iteration constructs
provide no essential gain in computational power. On the
other hand, such constructs are often convenient. Design
some iteration constructs, give examples of their use, and
show how to implement them as derived expressions.

|#

(load-ex "4.8")

#| Answer 

For reference in C:
  do-while loop -- do { statement(s); } while(condition);
  for loop -- (for init; condition; increment) { statement(s); }

Our api:

  ;;; repeatedly executes the given statements while the given condition holds.
  (do-while condition statements)

  ;;; each value to loop over takes:
  ;;; * name
  ;;; * initial-value
  ;;; * an expression that has access to the value and returns true or false
  ;;;   if false, statements are no longer executed
  ;;; * after statements are executed an expression that has access to the value
  ;;;   and returns the new value of value.
  (for ([a-name a-initial-value a-condition? a-incrementer]
        [b-name b-initial-value b-condition? b-intrementer]
        . . .)
       statements)
|#

#| Answer |#

(define (make-named-let name clauses body)
  (list 'let name clauses body))

(define (make-thunk-application proc)
  (make-application proc (list)))

(define (make-thunk body)
  (make-lambda (list) body))

(define (make-and exprs)
  (cons 'and exprs))

(define (is-do-while? exp) (tagged-list? exp 'do-while))
(define (is-for? exp) (tagged-list? exp 'for))

(define (transform-do-while exp)
  (define (do-while-condition exp) (cadr exp))
  (define (do-while-statements exp) (cddr exp))
  (let ([condition (do-while-condition exp)]
        [statements (do-while-statements exp)]
        [f (gensym)])
    (make-thunk-application
      (make-thunk
        (list 
          (make-define f (list (make-thunk
                                 (append statements
                                         (list (make-if condition 
                                                    (make-thunk-application f)
                                                    (make-thunk-application 'void)))))))
          (make-thunk-application f))))))

(define (transform-for exp)
  (define (for-clauses exp) (cadr exp))
  (define (for-body exp) (cddr exp))
  (define (clause-name clause) (car clause))
  (define (clause-initial-value clause) (cadr clause))
  (define (clause-condition clause) (caddr clause))
  (define (clause-iterator clause) (cadddr clause))
  (let ([clauses (for-clauses exp)]
        [body (for-body exp)]
        [f (gensym)])
    (make-named-let 
      f
      (map (lambda (c) (list (clause-name c) (clause-initial-value c))) clauses)
      (make-if (make-and (map clause-condition clauses))
               (sequence->exp 
                  (append
                    body
                    (list 
                      (make-application
                        f
                        (map clause-iterator clauses)))))
               (make-thunk-application 'void)))))

(define eval-48 eval)
(set! eval (lambda (exp env)
  (cond [(is-do-while? exp) (eval (transform-do-while exp) env)]
        [(is-for? exp) (eval (transform-for exp) env)]
        [else (eval-48 exp env)])))

#| Tests |#
(define-test (eval-one '(begin
                          (define x 5)
                          (do-while (> x 0) (set! x (- x 1)))
                          x))
             0)

(define-test (eval-one '(begin
                          (define x 0)
                          (for ([i 0 (< i 5) (+ i 1)])
                            (set! x (+ x 2)))
                          x))
             10)
