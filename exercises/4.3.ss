#|

Exercise 4.3: Rewrite "eval" so that the dispatch is done in
data-directed style. Compare this with the data-directed
differentiation procedure of Exercise 2.73. (You may use the
"car" of a compound expression as the type of the
expression, as is appropriate for the syntax implemented in
this section.)

|#

#| Answer 

There are two kinds of expressions.
  explictly tagged -- quoted, assignment, definition, if, lambda, begin, cond
  implicit -- self-evaluating, variable, application

application is defined as some expression whose tag has no known eval operation.
Since application's type is defined in terms of eval we'll make the compromise
that the generic eval operation has knowledge of applications.

Some requirements we'll implement are:
  * eval has no knowledge of types (other than applications).
  * all expressions have a "type" and "contents".

Relying on the "wishful thinking" strategy introduced by section 2.1.1, I'll
assume this infrastructure can be implemented.
  (put op type proc) -- defines a generic operation
  (get op type) -- returns a generic operation or #f
  (put-implicit pred type) -- defines an implicit type
  (get-implicit exp) -- returns an implicit type or #f

Thus the system will look something like:

|#

;;; typical implicit expression
(define (install-self-evaluating-package)
  (define (self-evaluating? exp)
    (cond [(number? exp) #t]
          [(string? exp) #t]
          [else #f]))
  (define (eval-self-evaluating exp) exp)
  ;;; interface
  (put-implicit self-evaluating? 'self-evaluating)
  (put 'eval '(self-evaluating) eval-self-evaluating))

;;; typical explictly tagged expression
(define (install-cond-package)
  (define (cond-clauses exp) ...)
  (define (cond-else-clause? clause) ...)
  (define (cond-predicate clause) ...)
  (define (cond-actions clause) ...)
  (define (cond->if exp) ...)
  (define (expand-clauses clauses) ...)
  (define (eval-cond exp)
    (eval (cond->if exp) env))
  ;;; interface
  (put 'eval '(cond) eval-cond))

;;; special case implicit -- application
(define (install-application-package)
  (define (eval-application exp)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))
  (put 'eval '(application) eval-application))

#| utilities |#
(define (type-tag datum)
  (let ([implicit-type (get-implicit datum)])
    (if implicit-type
        implicit-type
        (if (pair? datum)
            (car datum)
            (error "type-tag" "bad tagged datum" datum)]))))

(define (contents datum)
  (let ([implicit-type (get-implicit datum)])
    (if implicit-type
        datum
        (if (pair? datum)
            (cdr datum)
            (error "contents" "bad tagged datum")))))

(define (eval exp env)
  (let* ([type (type-tag exp)]
         [eval-op (get 'eval (list type))]
         [fallback-eval-op (get 'eval '(application))])
    (cond [eval-op (eval-op (contents exp))]
          [fallback-eval-op (fallback-eval-op exp)]
          [error "eval" "no eval known for" type])))

#| Thoughts

This architecture seems nice because it isolates a lot of selectors, etc. that
shouldn't be global.

Unlike section 2, in this data-directed system the data structures and the user
interface are one in the same: scheme code. Unfortunately this means our data
structure is frozen by requirement. A more robust approach might be to create a
parser which takes the scheme code and transforms it into an abstract syntax
tree. This tree could then be passed to a data-directed eval system with no
special cases. However application is still defined in terms of available
specific eval operations, so in some sense it might be leaking the concept of
eval into a parser.

It could be interested to eliminate derived expressions. Then you could pick
and choose packages with no dependencies.

Note: Our implementation of the special case for application technically allows
the user to use syntax like "(application + 2 2)" as well as "(+ 2 2)". We've
basically an alternative version of "apply".

|#
