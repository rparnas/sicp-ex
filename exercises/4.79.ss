#|

Exercise 4.79: When we implemented the Lisp evaluator in
Section 4.1, we saw how to use local environments to avoid
name conflicts between the parameters of procedures. For
example, in evaluating

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)

there is no confusion between the "x" in "square" and the
"x" in "sum-of-squares", because we evaluate the body of
each procedure in an environment that is specially
constructed to contain bindings for the local variables. In
the query system, we used a different strategy to avoid name
conflicts in applying rules. Each time we apply a rule we
rename the variables with new names that are guaranteed to
be unique. The analogous strategy for the Lisp evaluator
would be to do away with local environments and simply
rename the variables in the body of a procedure each time we
apply the procedure.

Implement for the query language a rule-application method
that uses environments rather than renaming. See if you can
build on your environment structure to create constructs in
the query language for dealing with large systems, such as
the rule analog of block-structured procedures. Can you
relate any of this to the problem of making deductions in a
context (e.g., "If I supposed that P were true, then I would
be able to deduce A and B.") as a method of problem solving?
(This problem is open-ended. A good answer is probably worth
a Ph.D.)

|#

(load-ex "4.66") ; without loop detection

#| Answer

When a procedure application finishes pop the environment frame associated with
that application. The value returned by the procedure may be dropped, saved in
the next environment out, or passed along as a return value.

Something similar must happen for rule application.

So I didn't have to rename everything I refer to environments as "frames" and
what would be a frame in an environment a "framelet".

In a scheme system, the query system seems like a procedure with multiple named
arguments and multiple named returns values. Except it is like the propagation
of constraints system where the inputs and outputs may be any variable.

In the new system, there can be a naming conflict between the query-pattern and
the rule itself. These can be impossible to avoid in the case of a rule that
causes recursive rule applications.

It seems like I need to rename the variables in the query to avoid naming
conflicts. This feels like cheating the spirit of the exercise prompt, however
temporarily renaming the variables in the query is better than renaming the rule
because (1) it is temporary (2) the names don't have to be uniquely generated,
just tagged (3) the resultant frame from the rule application is not poluted
with variables that were bound just to work through the application.

Currently, most of the regression tests pass except for (last-pair (1 2 3) ?x)
from 4.62. I'm not sure if the overall approach is on the correct track and I'm
not sure if these changes should invalidate any previous syntax, especially
involving recursion or the return of infinite results.

|#

#| 4.4.4.4 Rules and Unification |#
(define (apply-a-rule rule pattern frame)
  (let* ([clean-pattern (instantiate pattern frame (lambda (v f) (append '(? out) (cdr v))))]
         [rule-frame (cons '() frame)]
         [unify-result (unify-match clean-pattern (conclusion rule) rule-frame)])
    (if (eq? unify-result 'failed)
        the-empty-stream
        (stream-map pop 
                    (qeval (rule-body rule) (singleton-stream unify-result))))))

(define (tag-variables-in exp tag)
  (define (tree-walk exp)
    (cond [(var? exp)
           (var-add-tag exp tag)]
          [(pair? exp)
           (cons (tree-walk (car exp))
                 (tree-walk (cdr exp)))]
          [else exp]))
  (tree-walk exp))

(define (var-add-tag var tag)
  (make-new-variable var tag))
(define (var-rem-tag var)
  (cons '? (cddr var)))
(define (var-tag var)
  (if (= (length var) 3)
      (cadr var)
      #f))

(define (pop frame)
  (define (iter result bindings)
    (if (null? bindings)
        result
        (let* ([binding (car bindings)]
               [var (binding-variable binding)]
               [val (binding-value binding)])
          (if (eq? (var-tag var) 'out)
              (iter (extend (var-rem-tag var)
                            (instantiate val frame (lambda (v f) v))
                            result)
                    (cdr bindings))
              (iter result (cdr bindings))))))
  (iter (cdr frame) (car frame)))

#| 4.4.4.8 Frames and Bindings |#
(define (binding-in-frame variable frame)
  (if (null? frame)
      #f
      (assoc variable (car frame))))
  #|(define (iter framelets)
    (if (null? framelets)
        #f
        (let ([result (assoc variable (car framelets))])
          (or result (iter (cdr framelets))))))
  (iter frame))|#

(define (extend variable value frame)
  (let ([binding (make-binding variable value)])
    (if (null? frame)
        (list (list binding))
        (cons (cons binding
                    (car frame)) 
              (cdr frame)))))

#| Tests

A 4.57 regression test fails b/c free variables in results have different names.

A 4.62 regression test fails legitimately.

|#

(define (apply-a-rule rule pattern frame)
  (let* ([clean-pattern (instantiate pattern frame (lambda (v f) (append '(? out) (cdr v))))]
         [rule-frame (cons '() frame)]
         [unify-result (unify-match clean-pattern (conclusion rule) rule-frame)])
    (if (eq? unify-result 'failed)
        the-empty-stream
        (stream-map pop 
          (let ([query 
                (instantiate (rule-body rule) 
                 unify-result 
                 (lambda (v f)
                   (let ([binding (binding-in-frame v unify-result)])
                     (if binding
                         (binding-value v)
                         v))))]
                [frame-stream (singleton-stream unify-result)])
            (qeval query frame-stream))))))

#| 4.62 -- old

(define rule '(rule (last-pair ((? head) . (? rest)) ((? last-pair))) (last-pair (? rest) ((? last-pair)))))
(define query-pattern '(last-pair (2 (? x)) (3)))
(define query-frame '())
[define clean-rule (rename-variables-in rule)]
[define unify-result (unify-match query-pattern (conclusion clean-rule) query-frame)]
; (
;   ((? 2 last-pair) . 3) 
;   ((? 2 rest) (? x)) 
;   ((? 2 head) . 2)
; )
[define result (qeval (rule-body clean-rule) (singleton-stream unify-result))]
(
  (
    ((? 6 last-pair) . 3)
    ((? x) ? 6 last-pair)
    ((? 5 last-pair) . 3)
    ((? 5 rest) (? x))
    ((? 5 head) . 2)
  )
)

|#

#| 4.62 -- new

(define rule '(rule (last-pair ((? head) . (? rest)) ((? last-pair))) (last-pair (? rest) ((? last-pair)))))
(define pattern '(last-pair (2 (? x)) (3)))
(define frame '())

[define clean-pattern (instantiate pattern frame (lambda (v f) (append '(? out) (cdr v))))]
; (last-pair (2 (? out x)) (3))
[define rule-frame (cons '() frame)]
[define unify-result (unify-match clean-pattern (conclusion rule) rule-frame)]
; (
;   (
;     [(? last-pair) . 3]
;     [(? rest) (? out x)]
;     [(? head) . 2]
;   )
; )

;;; rule body is -- (last-pair (? rest) ((? last-pair)))

[define result (qeval (rule-body rule) (singleton-stream unify-result))]
;(
;  (
;    (
;      [(? last-pair) . 3]
;      [(? rest) (? out x)]
;      [(? head) . 2]
;    )
;  )
;)


|#