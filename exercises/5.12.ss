#|

Exercise 5.12: The simulator can be used to help determine
the data paths required for implementing a machine with a
given controller. Extend the assembler to store the
following information in the machine model:

* a list of all instructions, with duplicates removed,
sorted by instruction type ("assign", "goto", and so on);

* a list (without duplicates) of the registers used to hold
entry points (these are the registers referenced by "goto"
instructions);

* a list (without duplicates) of the registers that are
"save"d or "restore"d;

* for each register, a list (without duplicates) of the
sources from which it is assigned (for example, the sources
for register "val" in the factorial machine of Figure 5.11
are "(const 1)" and "((op *) (reg n) (reg val))").

Extend the message-passing interface to the machine to
provide access to this new information. To test your
analyzer, define the Fibonacci machine from Figure 5.12 and
examine the lists you constructed.

|#

(load-ex "5.11")

#| Answer 

The prompt suggests keeping track of "registers used to hold entry points" via
"registers referenced by 'goto'" however a more robust static analysis might
have at least some type analysis to ensure that the register used in a goto
instruction actually has a label.

|#


;;; patch to 5.1
(define make-new-machine-51 make-new-machine)
(define (make-new-machine)
  (let ([old (make-new-machine-51)]
        [instructions-used '()]
        [entry-point-registers '()]
        [saved-or-restored-registers '()]
        [register-sources '()])
    (define (add-used-instruction inst)
      (set! instructions-used (add-unique-and-sort inst instructions-used)))
    (define (add-entry-point-register reg)
      (set! entry-point-registers (add-unique-and-sort reg entry-point-registers)))
    (define (add-saved-or-restored-register reg)
      (set! saved-or-restored-registers (add-unique-and-sort reg saved-or-restored-registers)))
    (define (add-register-source reg src) 
      (if (not (assoc reg register-sources))
          (set! register-sources (cons (cons reg '()) register-sources))
          (void))
      (let ([sources (assoc reg register-sources)])
        (set-cdr! sources (add-unique src (cdr sources)))))
    (define (dispatch message)
      (cond [(eq? message 'add-used-instruction) add-used-instruction]
            [(eq? message 'instructions-used) instructions-used]
            [(eq? message 'add-entry-point-register) add-entry-point-register]
            [(eq? message 'entry-point-registers) entry-point-registers]
            [(eq? message 'add-saved-or-restored-register) add-saved-or-restored-register]
            [(eq? message 'saved-or-restored-registers) saved-or-restored-registers]
            [(eq? message 'add-register-source) add-register-source]
            [(eq? message 'register-sources) register-sources]
            [else (old message)]))
    dispatch))

;;; patch to 5.10
(define make-execution-procedure-510 make-execution-procedure)
(define (make-execution-procedure inst labels machine pc flag stack ops)
  ((machine 'add-used-instruction) (car inst))
  (make-execution-procedure-510 inst labels machine pc flag stack ops))

;;; patch to 5.1
(define make-goto-51 make-goto)
(define (make-goto inst machine labels pc)
  (let ([dest (goto-dest inst)])
    (if (register-exp? dest)
      ((machine 'add-entry-point-register) (cadr dest))))
  (make-goto-51 inst machine labels pc))

;;; patch to 5.11
(define make-save-511 make-save)
(define (make-save inst machine stack pc)
  ((machine 'add-saved-or-restored-register) (stack-inst-reg-name inst))
  (make-save-511 inst machine stack pc))

;;; patch to 5.11
(define make-restore-511 make-restore)
(define (make-restore inst machine stack pc)
  ((machine 'add-saved-or-restored-register) (stack-inst-reg-name inst))
  (make-restore-511 inst machine stack pc))

;;; patch to 5.1
(define make-assign-51 make-assign)
(define (make-assign inst machine labels operations pc)
  ((machine 'add-register-source) (assign-reg-name inst) (assign-value-exp inst))
  (make-assign-51 inst machine labels operations pc))

;;; utilities
(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
(define (add-unique-and-sort x ls)
  (if (member x ls)
      ls
      (sort symbol<? (cons x ls))))
(define (add-unique x ls)
  (if (member x ls)
      ls
      (cons x ls)))

#| Tests -- fib-machine from 5.6 |#

(define-test ((fib-machine) 'instructions-used)
             '(assign branch goto restore save test))
(define-test ((fib-machine) 'entry-point-registers)
             '(continue))
(define-test ((fib-machine) 'saved-or-restored-registers)
             '(continue n val))
(define-test ((fib-machine) 'register-sources)
             '((val ((reg n)) 
                    ((op +) (reg val) (reg n)))
               (n ((reg val))
                  ((op -) (reg n) (const 2))
                  ((op -) (reg n) (const 1)))
               (continue ((label afterfib-n-2))
                         ((label afterfib-n-1))
                         ((label fib-done)))))
