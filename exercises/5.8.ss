#|

Exercise 5.8: The following register-machine code is
ambiguous, because the label "here" is defined more than
once:

start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there

With the simulator as written, what will the contents of
register "a" be when control reaches "there"? Modify the
"extract-labels" procedure so that the assembler will signal
an error if the same label name is used to indicate two
different locations.

|#

(load-ex "5.6")

#| Answer 

As written, jumping to "here" jumps to the first occurrence of the label,
meaning "a" will have a value of 3.

|#

;;; modified from 5.1
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
        (lambda (insts labels)
          (let ([next-inst (car text)])
            (cond [(and (symbol? next-inst) (assoc next-inst labels))
                   (error "assemble" "redundant label" next-inst)]
                  [(symbol? next-inst)
                   (receive insts
                            (cons (make-label-entry next-inst insts)
                                  labels))]
                  [else 
                   (receive (cons (make-instruction next-inst)
                                  insts)
                            labels)]))))))

#| Tests 

> (define mach (make-machine
    '(a)
    (list)
    '(start
      (goto (label here))
      here
      (assign a (const 3))
      (goto (label there))
      here
      (assign a (const 4))
      (goto (label there))
      there)))
Exception in assemble: redundant label with irritant here
Type (debug) to enter the debugger.

|#