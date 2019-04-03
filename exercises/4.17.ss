#|

Exercise 4.17: Draw diagrams of the environment in effect
when evaluating the expression <e3> in the procedure in the
text, comparing how this will be structured when definitions
are interpreted sequentially with how it will be structured
if definitions are scanned out as described. Why is there an
extra frame in the transformed program? Explain why this
difference in environment structure can never make a
difference in the behavior of a correct program. Design a
way to make the interpreter implement the "simultaneous"
scope rule for internal definitions without constructing the
extra frame.

|#

(load-ex "4.16")

#| Answer

(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

global[(f *)______________________________]
          |         ^                    ^
          V         |                    |
         (f (body) (env))    lambda-frame[<vars> (u <e1>) (v <e2>)]

(lambda <vars>
  (let ((u '*unassigned)
        (v '*unassigned))
    (set! u <e1>)
    (set! v <e2>)
    <e3>)

global[(f *)______________________________]
          |         ^                    ^
          V         |                    |
         (f (body) (env))    lambda-frame[<vars>]
                                         ^
                                         |   
                                let-frame[(u <e1>) (v <e2>)]

|#

(define (scan-out-defines body)
  (define (iter defines result rest)
    (if (null? rest)
        (append defines result)
        (let ([first (car rest)])
          (if (definition? first)
              (let ([var (definition-variable first)]
                    [val (definition-value first)])
                (iter (append defines (list (make-definition var ''*unassigned*)))
                      (append result (list (make-assignment var val)))
                      (cdr rest)))
              (iter defines 
                    (append result (list first)) 
                    (cdr rest))))))
  (iter '() '() body))

#| Tests

> (define test-exp '(lambda (vars) (define u e1) (define v e2) e3))
> (define test-proc (eval-one test-exp))
> (procedure-body test-proc)
((define u '*unassigned*)
  (define v '*unassigned*)
  (set! u e1)
  (set! v e2)
  e3)

|#