#|

Exercise 4.8: "Named "let"" is a variant of "let" that has
the form

(let <var> <bindings> <body>)

The <bindings> and <body> are just as in ordinary "let",
except that <var> is bound within <body> to a procedure
whose body is <body> and whose parameters are the variables
in the <bindings>. Thus, one can repeatedly execute the
<body> by invoking the procedure named <var>. For example,
the iterative Fibonacci procedure (Section 1.2.2) can be
rewritten using named "let" as follows:

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

Modify "let->combination" of Exercise 4.6 to also support
named "let".

|#

(load-ex "4.7")

#| Answer |#

(define (make-define name seq) (cons 'define (cons name seq)))

(define (named-let->combination exp)
  (define (let-name exp) (cadr exp))
  (define (let-clauses exp) (caddr exp))
  (define (let-body exp) (cdddr exp))
  (define (let-clause-var clause) (car clause))
  (define (let-clause-exp clause) (cadr clause))
  (let ([name (let-name exp)]
        [clauses (let-clauses exp)]
        [body (let-body exp)])
    (make-application
      (make-lambda (list)
                   (list (make-define name 
                                (list (make-lambda (map let-clause-var clauses)
                                                   body)))
                         (make-application name
                                           (map let-clause-exp clauses))))
      (list))))

(define let->combination-old let->combination)

(set! let->combination (lambda (exp)
  (cond [(null? (cdr exp)) (error "let" "invalid syntax" exp)]
        [(list? (cadr exp)) (let->combination-old exp)]
        [(symbol? (cadr exp)) (named-let->combination exp)]
        [else (error "let" "invalid syntax" exp)])))

#| Tests |#
(define-test (eval-one '(let f () 0)) 
                        0)
(define-test (eval-one '(let f ([x 5]) (if (= x 0) 'done (f (- x 1)))))
                       'done)
(define-test (eval-one '(let f ([a 2]) (* a 2))) 
                        4)
(define-test (eval-one '(let fib-iter ([a 1] [b 0] [count 0])
                          (if (= count 0)
                              b
                              (fib-iter (+ a b) a (- count 1)))))
              0)
(define-test (eval-one '(let fib-iter ([a 1] [b 0] [count 1])
                          (if (= count 0)
                              b
                              (fib-iter (+ a b) a (- count 1)))))
              1)
(define-test (eval-one '(let fib-iter ([a 1] [b 0] [count 2])
                          (if (= count 0)
                              b
                              (fib-iter (+ a b) a (- count 1)))))
              1)
(define-test (eval-one '(begin
                          (define fib (lambda (n)
                            (let fib-iter ([a 1] [b 0] [count n])
                              (if (= count 0)
                                  b
                                  (fib-iter (+ a b) a (- count 1))))))
                          (fib 7)))
              13)
