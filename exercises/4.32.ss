#|

Exercise 4.32: Give some examples that illustrate the
difference between the streams of Chapter 3 and the "lazier"
lazy lists described in this section. How can you take
advantage of this extra laziness?

|#

(load-ex "4.27") ; skip over 4.31's

#| Code from book |#

(define setup-environment-41 setup-environment)

(define (setup-environment)
  (let ([env (setup-environment-41)])
    (define (add exp) (eval exp env))
    (add '(define (cons x y)
             (lambda (m) (m x y))))
    (add '(define (car z) 
             (z (lambda (p q) p))))
    (add '(define (cdr z) 
             (z (lambda (p q) q))))
    (add '(define (list-ref items n)
             (if (= n 0)
                 (car items)
                 (list-ref (cdr items) (- n 1)))))
    (add '(define (m proc items)
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

#| Answer 

These lazy lists are different in that the system considers them regular lists
and thus they can be used with any standard operations. Also both the car and
cdr is lazy. The text points out that this is advantangeous for the differential
equation examples because the code can more closely match the source
mathematical notation.

Many complex data structures can be built up using cons with a lazy cons you can
basically create a lazy anything.

|#

#| Tests

1 regression tests fail:
  * a 4.20  test returns a cons, but this is now represented differently.

> (eval-one '(begin
  (define (integral integrand initial-value dt)
    (define int
      (cons initial-value
            (add-lists (scale-list integrand dt)
                       int)))
    int)
  (define (solve f y0 dt)
    (define y (integral dy y0 dt))
    (define dy (map f y))
    y)
  (list-ref (solve (lambda (x) x) 1 0.001) 1000)))
2.716923932235896

|#

(define-test (eval-one '(begin
  (define a (cons 1 2))
  (car a)))
  1)

(define-test (eval-one '(begin
  (define a (cons 1 2))
  (cdr a)))
  2)

(define-test (eval-one '(list-ref ones 1000)) 1)

(define-test (eval-one '(list-ref integers 17)) 18)
