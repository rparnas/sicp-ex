#|

Exercise 4.34: Modify the driver loop for the evaluator so
that lazy pairs and lists will print in some reasonable way.
(What are you going to do about infinite lists?) You may
also need to modify the representation of lazy pairs so that
the evaluator can identify them in order to print them.

|#

(load-ex "4.33")

#| Answer 

I did a proof of concept prototype for printing lazy values. Some examples are
in the test section.

Areas for further exploration:
  * How should top level interaction affect the state of data? Should you be
    able to evaluate lazy expressions without triggering memoization?
  * Can certain data structures be detected and treated differently?
  * Can an infinite list be detected or treated differently somehow?
  * How useful is displaying a lazy expression when you can't see the 
    environment?
  * How useful would pretty-printed procedure bodys and environments be?
  * What does the developer want to see and what do they want to tune out?

|#

(define (my-print x)
  (cond [(is-lazy-cons? x) (format "#<cons ~a . ~a>" 
                            (my-print (lazy-cons-car x))
                            (my-print (lazy-cons-cdr x)))]
        [(thunk? x) (format "#<lazy ~a>" (thunk-exp x))]
        [(evaluated-thunk? x) (format "#<memo ~a>" (thunk-value x))]
        [(primitive-procedure? x) "#<primitive procedure>"]
        [(compound-procedure? x) "#<compound procedure>"]
        [else x]))

;;; re-do the setup-environment patch from 4.32 which is on top of 4.1
(define (setup-environment)
  (let ([env (setup-environment-41)])
    (define (add exp) (eval exp env))
    (add '(define (list-ref items n)
             (if (= n 0)
                 (car items)
                 (list-ref (cdr items) (- n 1)))))
    (add '(define (map proc items)
            (if (null? items)
                '()
                (cons (proc (car items))
                      (map proc (cdr items))))))
    (add '(define (scale-list items factor)
            (map (lambda (x) (* x factor))
                 items)))
    (add '(define (add-lists list1 list2)
            (cond [(null? list1) list2]
                  [(null? list2) list1]
                  [else (cons (+ (car list1) (car list2))
                              (add-lists (cdr list1) (cdr list2)))])))
    (add '(define ones (cons 1 ones)))
    (add '(define integers (cons 1 (add-lists ones integers))))
    env))

;;; Updated cons
(define (is-car? exp) (tagged-list? exp 'car))
(define (is-cdr? exp) (tagged-list? exp 'cdr))
(define (is-cons? exp) (tagged-list? exp 'cons))

(define (is-lazy-cons? exp) (tagged-list? exp 'lazy-cons))
(define (lazy-cons-car exp) (cadr exp))
(define (lazy-cons-cdr exp) (caddr exp))
(define (lazy-cons-env exp) (cadddr exp))

(define (eval-car exp env)
  (let* ([x (eval (cadr exp) env)]
         [f (force-it x)])
    (if (is-lazy-cons? f)
        (cadr f)
        (error "car" "must car a list"))))

(define (eval-cdr exp env)
  (let* ([x (eval (cadr exp) env)]
         [f (force-it x)])
    (if (is-lazy-cons? f)
        (caddr f)
        (error "cdr" "must car a list"))))

(define (make-lazy-cons x y env)
  (list 'lazy-cons (delay-it x env) (delay-it y env)))

(define eval-433 eval)
(set! eval (lambda (exp env)
  (cond [(is-cons? exp) (make-lazy-cons (cadr exp) (caddr exp) env)]
        [(is-car? exp) (eval-car exp env)]
        [(is-cdr? exp) (eval-cdr exp env)]
        [else (eval-433 exp env)])))

#| Tests

Same regression test failures as 4.33

4.1:
  expected -- (+ 2 2)
  actual -- (lazy-cons...
  =======================
  > (define e (setup-environment))
  > (eval '(define x '(+ 2 2)) e)
  > (force-it (eval '(car x) e))
  +
  > (force-it (eval '(car (cdr x)) e))
  2
  (force-it (eval '(car (cdr (cdr x))) e))
  2
  > (force-it (eval '(cdr (cdr (cdr x))) e))
  ()
  > (my-print (eval-one ''(+ 2 2)))
  "#<cons #<lazy '+> . #<lazy (cons '2 (cons '2 '()))>>"

4.20:
  expected -- (false . true)
  actual -- (cons (even? x) (odd? x))))
  ======================================
  > (define e (setup-environment))
  > (eval '(define x (begin (define (f x) (letrec  ([even? (lambda (n) (if (= n 0) true (odd? (- n 1))))] [odd? (lambda (n) (if (= n 0) false (even? (- n 1))))]) (cons (even? x) (odd? x)))) (f 7))) e)
  > (force-it (eval '(car x) e))
  false
  > (force-it (eval '(cdr x) e))
  true

  > (my-print (eval-one 'ones))
  "#<cons #<lazy 1> . #<lazy ones>>"

  > (my-print (eval-one 'integers))
  "#<cons #<lazy 1> . #<lazy (add-lists ones integers)>>"

  > (my-print (eval-one '(cdr integers)))
  "#<cons #<lazy (+ (car list1) (car list2))> . #<lazy (add-lists (cdr list1) (cdr list2))>>"

  > (my-print (eval-one '(car (cdr integers))))
  2

  > (define e (setup-environment))
  > (eval '(define x (cons (+ 1 1) (+ 2 2))) e)
  > (my-print (eval 'x e))
  "#<cons #<lazy (+ 1 1)> . #<lazy (+ 2 2)>>"
  > (my-print (eval '(car x) e))
  "#<lazy (+ 1 1)>"
  > (my-print (force-it (eval '(car x) e)))
  2
  > (my-print (eval '(car x) e))
  "#<memo 2>"
|#
