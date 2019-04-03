#|

Exercise 4.6: "let" expressions are derived expressions,
because

(let ((<var_(1)>  <exp_(1)> )   ...  (<var_(n)>  <exp_(n)> ))
  <body>)

is equivalent to

((lambda (<var_(1)> ... <var_(n)> )
   <body>)
 <exp_(1)> 
   ... 
 <exp_(n)> )

Implement a syntactic transformation "let->combination" that
reduces evaluating "let" expressions to evaluating
combinations of the type shown above, and add the
appropriate clause to "eval" to handle "let" expressions.

|#

(load-ex "4.5")

#| Answer |#
(define (is-let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (define (let-clauses exp) (cadr exp))
  (define (let-body exp) (cddr exp))
  (define (let-clause-var clause) (car clause))
  (define (let-clause-exp clause) (cadr clause))
  (let ([clauses (let-clauses exp)]
        [body (let-body exp)])
    (make-application
       (make-lambda (map let-clause-var clauses) body)
       (map let-clause-exp clauses))))

(define eval-45 eval)
(set! eval (lambda (exp env)
  (cond [(is-let? exp) (eval (let->combination exp) env)]
        [else (eval-45 exp env)])))

#| Tests |#
(define-test (eval-one '(let () 5)) 5)
(define-test (eval-one '(let ([x 5]) x)) 5)
(define-test (eval-one '(let ([x (+ 2 2)]
                              [y (- 10 5)])
                          (+ x y)))
             9)
(define-test (eval-one '(let ([x 5])
                          (define y (+ x x))
                          y))
             10)
