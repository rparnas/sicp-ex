#|

Exercise 4.52: Implement a new construct called "if-fail"
that permits the user to catch the failure of an expression.
"if-fail" takes two expressions. It evaluates the first
expression as usual and returns as usual if the evaluation
succeeds. If the evaluation fails, however, the value of the
second expression is returned, as in the following example:

 ;;; Amb-Eval input: 
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)
 ;;; Starting a new problem 
 ;;; Amb-Eval value: 
 all-odd 

 ;;; Amb-Eval input: 
(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
 ;;; Starting a new problem 
 ;;; Amb-Eval value: 
 8 

|#

(load-ex "4.51")
(no-regression)

#| Answer 

The simple transformation to (amb exp 'fail) implements
this.

|#

(define (is-if-fail? x) (tagged-list? x 'if-fail))
(define (if-fail-exp x) (cadr x))
(define (if-fail-failover x) (caddr x))

(define (transform-if-fail x)
  (list 'amb
        (if-fail-exp x)
        (if-fail-failover x)))

(define analyze-451 analyze)
(set! analyze (lambda (exp)
  (cond [(is-if-fail? exp) (analyze (transform-if-fail exp))]
        [else (analyze-451 exp)])))

#| Tests |#
(define-test (eval-all
  '(if-fail (let ((x (one-from-set '(1 3 5))))
              (require (even? x))
              x)
            'all-odd))
  '(all-odd))

(define-test (eval-all
  '(if-fail (let ((x (one-from-set '(1 3 5 8))))
            (require (even? x))
            x)
          'all-odd))
  '(8 all-odd))
