#|

Exercise 4.16: In this exercise we implement the method just
described for interpreting internal definitions. We assume
that the evaluator supports "let" (see Exercise 4.6).

a. Change "lookup-variable-value" (Section 4.1.3) to signal
an error if the value it finds is the symbol "*unassigned*".

b. Write a procedure "scan-out-defines" that takes a
procedure body and returns an equivalent one that has no
internal definitions, by making the transformation described
above.

c. Install "scan-out-defines" in the interpreter, either in
"make-procedure" or in "procedure-body" (see Section 4.1.3).
Which place is better? Why?

|#

(load-ex "4.13")

#| Answer

If you put it in procedure-body scan-out-defines is run many times, but the
original verbatim definition of the procedure is peserved in your data
structure.

If you put it in make-procedure it is only run once. However this should be
considered a major architectural decision. Previously you parsed all user
expressions into your data structures in a verbatim manner and only performed
transformations on the fly during evaluation. Updating make-procedure breaks
this attribute of the system.

I think storing the verbatim body in the system's data structure is easier to
debug so I will take the procedure-body route.

|#

(define (make-definition var value)
  (list 'define var value))

(define (make-assignment var value)
  (list 'set! var value))

(define (procedure-body p) 
  (scan-out-defines (caddr p)))

(define (lookup-variable-value var env)
  (let ([frame (find-frame-with-binding var env)])
    (if frame
        (let ([value (frame-get-value frame var)])
          (if (eq? value '*unassigned*)
              (error "" "unassigned variable" var)
              value))
        (error "" "unbound variable" var))))

(define (scan-out-defines body)
  (define (iter lets result rest)
    (if (null? rest)
      (if (null? lets)
          body
          (let ([vars lets]
                [exps (map (lambda (x) ''*unassigned*) lets)]
                [body result])
            (list (make-let vars exps body))))
        (let ([first (car rest)])
          (if (definition? first)
            (let ([var (definition-variable first)]
                  [val (definition-value first)])
              (iter (append lets (list var))
                    (append result (list (make-assignment var val)))
                    (cdr rest)))
            (iter lets
                  (append result (list first))
                  (cdr rest))))))
  (iter '() '() body))

#| Tests

> (define env (setup-environment))
> (eval '(define x '*unassigned*) env)
(frame (x . *unassigned*) ...
> (eval 'x env)
Exception: unassigned variable with irritant x
Type (debug) to enter the debugger.

> (define test-exp '(lambda (vars) (define u e1) (define v e2) e3))
> (define test-proc (eval-one test-exp))
> (procedure-body test-proc)
((let ([u '*unassigned*] [v '*unassigned*])
   (set! u e1)
   (set! v e2)
   e3))

|#
