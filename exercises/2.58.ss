#|

Exercise 2.58: Suppose we want to modify the differentiation
program so that it works with ordinary mathematical
notation, in which "+" and "*" are infix rather than prefix
operators. Since the differentiation program is defined in
terms of abstract data, we can modify it to work with
different representations of expressions solely by changing
the predicates, selectors, and constructors that define the
representation of the algebraic expressions on which the
differentiator is to operate.

a. Show how to do this in order to differentiate algebraic
expressions presented in infix form, such as "(x + (3 * (x +
(y + 2))))". To simplify the task, assume that "+" and "*"
always take two arguments and that expressions are fully
parenthesized.

b. The problem becomes substantially harder if we allow
standard algebraic notation, such as "(x + 3 * (x + y +
2))", which drops unnecessary parentheses and assumes that
multiplication is done before addition. Can you design
appropriate predicates, selectors, and constructors for this
notation such that our derivative program still works?

|#

#| Answer a. infix, fully parameterized |#
(load-ex "2.57")
(no-regression)

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list a1 '+ a2)]))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list m1 '* m2)]))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

#| Tests for a. 

; [d/dx] x^2 = 2x
> (define t1 (make-exponentiation 'x 2))
> (deriv t1 'x)
(2 * x)

; [d/dx] ax^2 = 2ax
> (define t2 (make-product 'a t1))
> (deriv t2 'x)
(a * (2 * x))

; [d/dx] ax^2 + bx = 2ax + b
> (define t3 (make-sum t2 (make-product 'b 'x)))
> (deriv t3 'x)
((a * (2 * x)) + b)

; [d/dx] ax^2 + bx + c = 2ax + b
> (define t4 (make-sum t3 'c))
> (deriv t4 'x)
((a * (2 * x)) + b)

|#

#| Answer b. infix arbitrary length |#

; filters the given list by index
(define (filter-index f ls)
  (define (iter i result ls)
    (if (null? ls)
        result
        (iter (+ 1 i)
              (if (f i)
                  (cons (car ls) result) 
                  result)
              (cdr ls))))
  (reverse (iter 0 '() ls)))

; return all operators in an expression
(define (get-all-operators exp)
  (filter-index odd? exp))

; returns all operands in an expression
(define (get-all-operands exp)
  (filter-index even? exp))

; returns true if operator op0 should be handled first 
(define (op>= op0 op1)
  (cond [(eq? op0 op1) #t]
        [(and (eq? op0 '*) (eq? op1 '+)) #t]
        [(and (eq? op0 '+) (eq? op1 '*)) #f]
        [else (error 'op> "unexpected operators" op0 op1)]))

; returns the highest precedence operator in an expression
(define (get-op exp)
  (if (not (pair? exp))
      #f
      (car (sort op>= (get-all-operators exp)))))

; chops the list at the first instance of value
; e.g. given (define ls '(1 2 3 4 5 3))
; (chop 3 ls) is '((1 2) . (3 4 5 3))
(define (chop value ls)
  (define (iter before ls)
   (cond [(null? ls) (cons (reverse before) (list ls))]
         [(eq? (car ls) value) (cons (reverse before) (list (cdr ls)))]
         [else (iter (cons (car ls) before) (cdr ls))]))
  (iter '() ls))

; returns the first term of an expression
(define (get-first-term exp)
  (let* ([op (get-op exp)]
         [before (car (chop op exp))])
    (if (null? (cdr before))
        (car before)
        before)))

; returns the rest term of the expression
(define (get-rest-term exp)
  (let* ([op (get-op exp)]
         [rest (cadr (chop op exp))])
    (if (null? (cdr rest))
        (car rest)
        rest)))

; makes an expression assuming terms have been simplified.
(define (make terms op)
  (define (delimit terms op)
    (fold-right (lambda (a b)  
                  (if (null? b)
                      (cons a b)
                      (cons a (cons op b))))
              '()
              terms))
  (cond [(null? terms) 0]
        [(null? (cdr terms)) (car terms)]
        [else (delimit terms op)]))

; returns true if the given is not a number.
(define (not-number? x) (not (number? x)))

; Interface
(define (make-sum . args)
  (let* ([numbers (filter number? args)]
         [others (filter not-number? args)]
         [number (fold-right + (+) numbers)]
         [terms (cond [(= 0 number) others]
                      [else (append others (list number))])])
    (make terms '+)))
(define (sum? exp) (eq? '+ (get-op exp)))
(define addend get-first-term)
(define augend get-rest-term)

(define (make-product . args)
  (let* ([numbers (filter number? args)]
         [others (filter not-number? args)]
         [number (fold-right * (*) numbers)]
         [terms (cond [(= 0 number) (list 0)]
                      [(= 1 number) others]
                      [else (append others (list number))])])
  (make terms '*)))
(define (product? exp) (eq? '* (get-op exp)))
(define multiplier get-first-term)
(define multiplicand get-rest-term)

#| Infrastructure Tests |#
(define-test (filter-index odd? '(0 1 2 3 4 5)) 
             '(1 3 5))
(define-test (get-all-operators '(1 + 2 + 3 * 4)) 
            '(+ + *))
(define-test (get-all-operands '(1 + 2 + 3 * 4)) 
             '(1 2 3 4))
(define-test (op>= '+ '*) 
             #f)
(define-test (op>= '* '+) 
             #t)
(define-test (get-op '(1 + 2 + 3 * 4)) 
             '*)
(define-test (get-op '(1 + 2 + 3)) 
             '+)
(define-test (chop 2 '(1 2 3 4)) 
             '((1) (3 4)))
(define-test (chop 3 '(1 2 3 4)) 
             '((1 2) (4)))
(define-test (chop 4 '(1 2 3 4)) 
             '((1 2 3) ()))
(define-test (get-first-term '(1 + 2 + 3 * 4)) 
             '(1 + 2 + 3))
(define-test (get-first-term '(1 + 2 + 3)) 
             1)
(define-test (get-rest-term '(1 + 2 + 3 * 4)) 
             4)
(define-test (get-rest-term '(1 + 2 + 3)) 
             '(2 + 3))
(define-test (get-rest-term '(1 + 2 + 3 * 4 + 5)) 
             '(4 + 5))

#| Tests |#
(define-test (deriv (make-sum 'x 3) 'x) 
             1)
(define-test (deriv (make-product 'x 'y) 'x) 
             'y)
(define-test (deriv (make-sum 'x (make-product 3 (make-sum 'x 'y 2))) 'x) 
             4)
(define-test (deriv (make-product 'x 'y (make-sum 'x 3)) 'x) 
             '((x * y) + (y * (x + 3))))

#| Notes 

TODO: Would be nice if prefix notation allowed arbitrary arg count.

TODO: Would be nice for both notations to simplify nested
operations of the same type.

|#