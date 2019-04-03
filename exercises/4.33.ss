#|

Exercise 4.33: Ben Bitdiddle tests the lazy list
implementation given above by evaluating the expression:

(car '(a b c))

To his surprise, this produces an error. After some thought,
he realizes that the "lists" obtained by reading in quoted
expressions are different from the lists manipulated by the
new definitions of "cons", "car", and "cdr". Modify the
evaluator's treatment of quoted expressions so that quoted
lists typed at the driver loop will produce true lazy lists.

|#

(load-ex "4.32")

#| Answer |#

(define (eval-quote exp env) 
  (define (quoted-transform x)
    (if (or (null? x) (not (pair? x)))
        `',x
        (let ([first (car x)]
              [rest (quoted-transform (cdr x))])
          `(cons ',first ,rest))))
  (let ([x (text-of-quotation exp)])
    (if (or (null? x) (not (pair? x)))
        x
        (eval (quoted-transform x) env))))

(define eval-432 eval)
(define (eval exp env)
  (cond [(quoted? exp) (eval-quote exp env)]
        [else (eval-432 exp env)]))

#| Tests 

2 regression tests fail:
  * a 4.1 test quotes a list, but this is now represented differently.
  * a 4.20 test returns a cons, but this is now represented differently.

|#

(define-test (eval-one ''()) '())

(define-test (eval-one '(car '(a))) 'a)
(define-test (eval-one '(cdr '(a))) '())

(define-test (eval-one '(car '(a b))) 'a)
(define-test (eval-one '(car (cdr '(a b)))) 'b)
(define-test (eval-one '(cdr (cdr '(a b)))) '())

(define-test (eval-one '(car '(a b c))) 'a)
