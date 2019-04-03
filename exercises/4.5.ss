#|

Exercise 4.5: Scheme allows an additional syntax for "cond"
clauses, "(<test> => <recipient>)". If <test> evaluates to a
true value, then <recipient> is evaluated. Its value must be
a procedure of one argument; this procedure is then invoked
on the value of the <test>, and the result is returned as
the value of the "cond" expression. For example

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

returns 2. Modify the handling of "cond" so that it supports
this extended syntax.

|#

(load-ex "4.4")

#| Answer 

This implementation has a flaw in that an arrow clause is transformed into
something which evaluates the predicate twice like [(pred) => proc] becomes [if
(pred) (proc (pred)... A derived expression containing a subexpression more than
once is a bad smell because that subexpression could have side effects and at
the very least it is redundat.

It is beyond the scope of this exercise but a more correct solution would be to
either forgo a derived expression or to evaluate the predicate once in a let
like:

(let [unique (pred)] (if unique (proc unique...

|#

(define (cond-is-arrow? clause) (tagged-list? (cdr clause) '=>))
(define (cond-arrow-proc clause) (caddr clause))

(define (make-application proc arg-list)
  (cons proc arg-list))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "cond" "else clause isn't last" clauses))
            (make-if (cond-predicate first)
                     (if (cond-is-arrow? first)
                         (make-application (cond-arrow-proc first)
                                           (list (cond-predicate first)))
                         (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))

#| Tests |#
(define-test (eval-one '(cond [2 => +] [else 1]))
             2)

(define-test (eval-one '(cond [false => +] [else 1]))
             1)