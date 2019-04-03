#|

Exercise 2.56: Show how to extend the basic differentiator
to handle more kinds of expressions. For instance, implement
the differentiation rule

d(u^n)            du
------ = nu^(n-1) --
  dx              dx

by adding a new clause to the "deriv" program and defining
appropriate procedures "exponentiation?", "base",
"exponent", and "make-exponentiation". (You may use the
symbol "**" to denote exponentiation.) Build in the rules
that anything raised to the power 0 is 1 and anything raised
to the power 1 is the thing itself.

|#

#| Book Code (Pass 1) |#
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        (else
          (error "deriv" "unknown type" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

#| Book Code (Pass 1) Tests 

> (deriv '(+ x 3) 'x)
(+ 1 0)
> (deriv '(* x y) 'x)
(+ (* x 0) (* 1 y))
> (deriv '(* (* x y) (+ x 3)) 'x)

|#

#| Book Code (Pass 2): Simplified Form |#
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
          (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

#| Book Code (Pass 2): Simplified Form Tests 

> (deriv '(+ x 3) 'x)
1
> (deriv '(* x y) 'x)
(+ (* x 0) (* 1 y))
> (deriv '(* (* x y) (+ x 3)) 'x)
(+ (* (* x y) 1) (* (+ (* x 0) (* 1 y)) (+ x 3)))

|#

#| Answer |#
(define (make-exponentiation b e) 
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) -1))))
        (else
          (error "deriv" "unknown type" exp))))

#| Tests |#

; [d/dx] x^2 = 2x
(define t1 (make-exponentiation 'x 2))
(define-test (deriv t1 'x) '(* 2 x))

; [d/dx] ax^2 = 2ax
(define t2 (make-product 'a t1))
(define-test (deriv t2 'x) '(* a (* 2 x)))

; [d/dx] ax^2 + bx = 2ax + b
(define t3 (make-sum t2 (make-product 'b 'x)))
(define-test (deriv t3 'x) '(+ (* a (* 2 x)) b))

; [d/dx] ax^2 + bx + c = 2ax + b
(define t4 (make-sum t3 'c))
(define-test (deriv t4 'x) '(+ (* a (* 2 x)) b))

#| Notes

This doesn't work for derivative of (y^2)...
> (deriv '(** y 2) 'x) ; (y^2)
(* 2 y)                ; 0

> (deriv '(* 2 (* (** x 2) (** y 2))) 'x)           ; 2(x^2)(y^2)
(* 2 (+ (* (** x 2) (* 2 y)) (* (* 2 x) (** y 2)))) ; 4xy^2

|#
