#|

Exercise 2.73: Section 2.3.2 described a program that
performs symbolic differentiation:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                   (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
        <more rules can be added here>
        (else (error "unknown expression type:
                      DERIV" exp))))

We can regard this program as performing a dispatch on the
type of the expression to be differentiated. In this
situation the "type tag" of the datum is the algebraic
operator symbol (such as "+") and the operation being
performed is "deriv". We can transform this program into
data-directed style by rewriting the basic derivative
procedure as

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

a. Explain what was done above. Why can't we assimilate the
predicates "number?" and "variable?" into the data-directed
dispatch?

b. Write the procedures for derivatives of sums and
products, and the auxiliary code required to install them in
the table used by the program above.

c. Choose any additional differentiation rule that you like,
such as the one for exponents (Exercise 2.56), and install
it in this data-directed system.

d. In this simple algebraic manipulator the type of an
expression is the algebraic operator that binds it together.
Suppose, however, we indexed the procedures in the opposite
way, so that the dispatch line in "deriv" looked like

((get (operator exp) 'deriv) (operands exp) var)

What corresponding changes to the derivative system are
required?

|#

#| Code book assumes we have |#
(define op-table (list))

(define (put op type proc) 
  (set! op-table (cons (list op type proc) op-table)))

(define (get op type)
  (let ([ret (find (lambda (x) 
                     (and (equal? op (car x))
                          (equal? type (cadr x))))
                   op-table)])
  (if ret
      (caddr ret)
      ret)))

#| Code from book |#
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

#| Code from book -- complex number system  |#
(define (install-rectangular-package)
  ;; interal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; interal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-an 'polar) r a))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

#| Code from book -- new derivatives system |#
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
                    (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

#| Answer

-- written by referencing ex. 2.56, 2.57, 2.58--
-- Extra Feature: simplify nested sums, nested products --

a. We seperated how to do derivatives for various types into
seperate functions. We can't include number? and same-
variable? because they are built-in types whose
implementation we can't change. All types in our system must
be tagged in a certain way.

b. sums and products

c. anything you want -- exponents

d. If it was like (get op proc) put would have to change to
(put op proc) everywhere it is used.

|#

(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s)
    (if (null? (cddr s))
        (cadr s)
        (apply make-sum (cdr s))))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (make-sum-inner args)      
    (define (flatten-sums args)
      (let* ([sums (filter sum? args)]
             [sum-terms (fold-right (lambda (a b) (append (contents a) b)) '() sums)]
             [others (filter (lambda (x) (not (sum? x))) args)])
        (if (null? sum-terms)
            others
            (flatten-sums (append sum-terms others)))))
    (let* ([all-args (flatten-sums args)]
           [numbers (filter number? all-args)]
           [others (filter (lambda (x) (not (number? x))) all-args)]
           [number (fold-right + (+) numbers)]
           [number-ls (if (= 0 number) '() (list number))]
           [terms (append others number-ls)])
      (if (null? (cdr terms))
          (car terms)
          (attach-tag '+ terms))))
  (put 'make-sum '+ make-sum-inner)
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-package)
  (define (multiplier p) (car p))
  (define (multiplicand p)
    (if (null? (cddr p))
        (cadr p)
        (apply make-product (cdr p))))
  (define (deriv-product exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                           (multiplicand exp))))
  (define (make-product-inner args)
    (define (flatten-products args)
      (let* ([ps (filter product? args)]
             [p-terms (fold-right (lambda (a b) (append (contents a) b)) '() ps)]
             [others (filter (lambda (x) (not (product? x))) args)])
        (if (null? p-terms)
            others
            (flatten-products (append p-terms others)))))
    (let* ([all-args (flatten-products args)]
           [numbers (filter number? all-args)]
           [others (filter (lambda (x) (not (number? x))) all-args)]
           [number (fold-right * (*) numbers)]
           [number-ls (if (= 1 number) 
                          '() 
                          (list number))]
           [terms (if (= 0 number)
                      '(0)
                      (append number-ls others))])
      (if (null? (cdr terms))
          (car terms)
          (attach-tag '* terms))))
  (put 'make-product '* make-product-inner)
  (put 'deriv '* deriv-product)
 'done)

(define (install-exponents-package)
  (define (base x) (car x))
  (define (exponent x) (cadr x))
  (define (deriv-exponentiation exp var)
    (make-product (exponent exp)
                  (make-exponentiation (base exp)
                                       (make-sum (exponent exp) -1))))
  (put 'make-exponentiation '**
    (lambda (b e)
      (cond [(and (number? e) (= e 0)) 1]
            [(and (number? e) (= e 1)) b]
            [else (attach-tag '** (list b e))])))
  (put 'deriv '** deriv-exponentiation)
  'done)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum . args)
  ((get 'make-sum '+) args))

(define (make-product . args)
  ((get 'make-product '*) args))

(define (make-exponentiation b e)
  ((get 'make-exponentiation '**) b e))

(define (sum? x)
  (and (not (number? x)) (not (symbol? x)) (eq? '+ (type-tag x))))

(define (product? x)
  (and (not (number? x)) (not (symbol? x)) (eq? '* (type-tag x))))

(define (exponentiation? x)
  (and (not (number? x)) (not (symbol? x)) (eq? '** (type-tag x))))

(install-rectangular-package)
(install-polar-package)
(install-sum-package)
(install-product-package)
(install-exponents-package)

#| Infrastructure Tests |#
(define-test (make-sum (make-sum 'x 1) 5) 
             '(+ x 6))
(define-test (make-sum (make-sum 'x 1) (make-sum 'y 1))
             '(+ x y 2))
(define-test (deriv (make-sum 'x 3) 'x)
             1)
(define-test (deriv (make-product 'x 'y) 'x) 
             'y)
(define-test (deriv (make-product (make-product 'x 'y) (make-sum 'x 3)) 'x)
             '(+ (* x y) (* y (+ x 3))))
(define-test (make-product (make-product 'x 'y) (make-product 'a 'b))
             '(* x y a b))

; [d/dx] x^2 = 2x
(define t1 (make-exponentiation 'x 2))
(define-test (deriv t1 'x) 
             '(* 2 x))

; [d/dx] ax^2 = 2ax
(define t2 (make-product 'a t1))
(define-test (deriv t2 'x)
             '(* 2 x a))

; [d/dx] ax^2 + bx = 2ax + b
(define t3 (make-sum t2 (make-product 'b 'x)))
(define-test (deriv t3 'x)
             '(+ (* 2 x a) b))

; [d/dx] ax^2 + bx + c = 2ax + b
(define t4 (make-sum t3 'c))
(define-test (deriv t4 'x)
             '(+ (* 2 x a) b))
