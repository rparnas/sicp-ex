#|

Exercise 5.37: One way to understand the compiler's
"preserving" mechanism for optimizing stack usage is to see
what extra operations would be generated if we did not use
this idea. Modify "preserving" so that it always generates
the "save" and "restore" operations. Compile some simple
expressions and identify the unnecessary stack operations
that are generated. Compare the code to that generated with
the "preserving" mechanism intact.

|#

(load-ex "5.33") ; skip 5.36 left-to-right evaluation

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if #t #|(and (needs-register? seq2 first-reg)
                        (modifies-register? seq1 first-reg))|#
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

#| Answer

I compared (+ 2 2) with and without preserving optimization disabled:

                                                                    |   (save continue)
                                                                    |   (save env)
                                                                    |   (save continue)
  (assign proc (op lookup-variable-value) (const +) (reg env))      |   (assign proc (op lookup-variable-value) (const +) (reg env))
                                                                    |   (restore continue)
                                                                    |   (restore env)
                                                                    |   (restore continue)
                                                                    |   (save continue)
                                                                    |   (save proc)
                                                                    |   (save env)
                                                                    |   (save continue)
  (assign val (const 2))                                            |   (assign val (const 2))
                                                                    |   (restore continue)
  (assign argl (op list) (reg val))                                 |   (assign argl (op list) (reg val))
                                                                    |   (restore env)
                                                                    |   (save argl)
                                                                    |   (save continue)
  (assign val (const 2))                                            |   (assign val (const 2))
                                                                    |    (restore continue)
                                                                    |   (restore argl)
  (assign argl (op cons) (reg val) (reg argl))                      |   (assign argl (op cons) (reg val) (reg argl))
                                                                    |   (restore proc)
                                                                    |   (restore continue)
  (test (op primitive-procedure?) (reg proc))                       |   (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))                                |   (branch (label primitive-branch6))
compiled-branch5                                                    | compiled-branch5
  (assign continue (label after-call4))                             |   (assign continue (label after-call4))
  (assign val (op compiled-procedure-entry) (reg proc))             |   (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))                                                  |   (goto (reg val))
primitive-branch6                                                   | primitive-branch6
                                                                    |   (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) |   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
                                                                    |   (restore continue)
after-call4                                                         | after-call4


|#