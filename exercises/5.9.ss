#|

Exercise 5.9: The treatment of machine operations above
permits them to operate on labels as well as on constants
and the contents of registers. Modify the
expression-processing procedures to enforce the condition
that operations can be used only with registers and
constants.

|#

(load-ex "5.8")

#| Answer 

Operations are valid in:
  - (assign <register-name> (op <operation-name>) <input_1>...<input_n>)
  - (perform (op <operation-name>) <input_1>...<input_n>)
  - (test (op <operation-name>) <input_1>...<input_n>)
  
  make-assign, make-perform, make-test call make-operation-exp.

|#

;;; modified from 5.1
(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp) operations)]
        [aprocs
         (map (lambda (e)
                (if (or (register-exp? e) (constant-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "assemble" 
                           "op argument must be register or constant"
                           e)))
              (operation-exp-operands exp))])
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

#| Tests 

> (make-machine
    '(a)
    (list (list 'display display))
    '(label
      (test (op display) (label label))
      (branch (label done))
      done))
Exception in assemble: op argument must be register or constant with irritant (label label)
Type (debug) to enter the debugger.

|#