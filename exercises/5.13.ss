#|

Exercise 5.13: Modify the simulator so that it uses the
controller sequence to determine what registers the machine
has rather than requiring a list of registers as an argument
to "make-machine". Instead of pre-allocating the registers
in "make-machine", you can allocate them one at a time when
they are first seen during assembly of the instructions.

|#

(load-ex "5.12")

#| Answer |#

;;; patches 5.1
(define make-machine-51 make-machine)
(define (make-machine . args) ; originally (lambda (register-names ops controller-text)...
  (cond [(= (length args) 2) 
         (make-machine-51 '() (car args) (cadr args))]
        [(= (length args) 3) 
         (make-machine-51 '() (cadr args) (caddr args))]
        [else (error "make-machine" "incorrect # of arguments")]))

;;; rewrite from 5.1 and 5.12
(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()]
        [instructions-used '()]
        [entry-point-registers '()]
        [saved-or-restored-registers '()]
        [register-sources '()])
    (let ([the-ops (list (list 'initialize-stack 
                               (lambda () (stack 'initialize))))]
          [register-table (list (list 'pc pc) (list 'flag flag))])
      (define (lookup-register name)
        ;;; create a register if not present
        (if (not (assoc name register-table))
            (set! register-table
                  (cons (list name (make-register name))
                        register-table))
            (void))
        ;;; return the register
        (cadr (assoc name register-table)))
      (define (execute)
        (let ([insts (get-contents pc)])
          (if (null? insts)
              (void)
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
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
        (cond [(eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)]
              [(eq? message 'install-instructions-sequence)
               (lambda (seq) (set! the-instruction-sequence seq))]
              [(eq? message 'get-register)
               lookup-register]
              [(eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops)))]
              [(eq? message 'stack) stack]
              [(eq? message 'operations) the-ops]
              [(eq? message 'add-used-instruction) add-used-instruction]
              [(eq? message 'instructions-used) instructions-used]
              [(eq? message 'add-entry-point-register) add-entry-point-register]
              [(eq? message 'entry-point-registers) entry-point-registers]
              [(eq? message 'add-saved-or-restored-register) add-saved-or-restored-register]
              [(eq? message 'saved-or-restored-registers) saved-or-restored-registers]
              [(eq? message 'add-register-source) add-register-source]
              [(eq? message 'register-sources) register-sources]
              [else (error "machine" "unknown request" message)]))
      dispatch)))

#| Tests -- see regression tests |#