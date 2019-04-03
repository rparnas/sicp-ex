#|

Exercise 5.15: Add instruction counting to the register
machine simulation. That is, have the machine model keep
track of the number of instructions executed. Extend the
machine model's interface to accept a new message that
prints the value of the instruction count and resets the
count to zero.

|#

(load-ex "5.14")

#| Answer 

Skipped resetting count to zero. count is outputted instead of printed.

|#

(define default-trace #f)

;;; rewrite of 5.13 and 5.14.
(set! make-new-machine (lambda ()
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()]
        [instructions-used '()]
        [entry-point-registers '()]
        [saved-or-restored-registers '()]
        [register-sources '()]
        [instructions-executed '()]
        [skip-breakpoints #f]
        [trace default-trace])
    (let ([the-ops (list (list 'initialize-stack 
                               (lambda () (stack 'initialize)))
                         (list 'print-stack-statistics
                               (lambda () (stack 'print-statistics))))]
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
              (let ([inst (car insts)])
                (if (and (not skip-breakpoints) (instruction-breakpoint inst))
                    (begin
                      (newline)
                      (display "break"))
                    (begin
                      (set! skip-breakpoints #f)
                      (if trace
                          (let ([str (instruction-text inst)])
                            (newline)
                            (if (not (symbol? str))
                                (display "  ")
                                (void))
                            (display str))
                          (void))
                      (if (symbol? (instruction-text inst))
                          (advance-pc pc)
                          (begin
                            (set! instructions-executed (append instructions-executed (list (instruction-text inst))))
                            ((instruction-execution-proc inst))))
                     (execute)))))))
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

      ;;; breakpointing
      (define (get-instruction-at label-name n)
        (define (iter insts n)
          (cond [(= n 0) insts]
                [(null? insts) (error "machine" "invalid skip amount" label-name n)]
                [else (iter (cdr insts) (- n 1))]))
        (let ([insts (memp (lambda (i) (equal? (instruction-text i) label-name)) the-instruction-sequence)])
          (if insts
              (iter insts n)
              (error "machine" "invalid label" label-name))))
      (define (set-breakpoint label-name n)
        (let ([insts (get-instruction-at label-name n)])
          (set-instruction-breakpoint! (car insts) #t)))
      (define (proceed)
        (set! skip-breakpoints #t)
        (execute))
      (define (cancel-breakpoint label n)
        (let ([insts (get-instruction-at label-name n)])
          (set-instruction-breakpoint! (car insts) #f)))
      (define (cancel-all-breakpoints)
        (for-each (lambda (i) (set-instruction-breakpoint! i #f))
                  the-instruction-sequence))
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
              [(eq? message 'instructions-executed) instructions-executed]
              [(eq? message 'instructions-executed-count) (length instructions-executed)]
              [(eq? message 'trace-on) (set! trace #t)]
              [(eq? message 'trace-off) (set! trace #f)]
              [(eq? message 'set-breakpoint) set-breakpoint]
              [(eq? message 'proceed) (proceed)]
              [(eq? message 'cancel-breakpoint) cancel-breakpoint]
              [(eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints)]
              [else (error "machine" "unknown request" message)]))
      dispatch))))

;;; new
(define (trace-on) (set! default-trace #t))
(define (trace-off) (set! default-trace #f))
(define (set-breakpoint machine label n) ((machine 'set-breakpoint) label n))
(define (proceed machine) (machine 'proceed))
(define (cancel-breakpoint machine label n) ((machine 'cancel-breakpoint) machine label n))
(define (cancel-all-breakpoints) (machine 'cancel-all-breakpoints))

;;; modified from 5.8
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
        (lambda (insts labels)
          (let* ([next-inst (car text)]
                 [next-insts (cons (make-instruction next-inst) insts)])
            (cond [(and (symbol? next-inst) (assoc next-inst labels))
                   (error "assemble" "redundant label" next-inst)]
                  [(symbol? next-inst)
                   (receive next-insts 
                            (cons (make-label-entry next-inst next-insts) labels))]
                  [else 
                   (receive next-insts 
                            labels)]))))))

;;; modified from 5.1
(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [ops (machine 'operations)])
    (for-each
      (lambda (inst)
        (if (symbol? (instruction-text inst))
            (void)
            (set-instruction-execution-proc!
              inst
              (make-execution-procedure
                (instruction-text inst) labels machine
                pc flag stack ops))))
      insts)))

;;; modified from 5.1
(define (make-register name)
  (let ([contents '*unassigned*]
        [trace #f])
    (define (dispatch message)
      (cond [(eq? message 'get) contents]
            [(eq? message 'set)
             (lambda (value)
              (if trace
                  (display (format " ; ~a: ~a -> ~a"
                                    name
                                    contents
                                    value))
                  (void))
              (set! contents value))]
            [(eq? message 'trace-on) (set! trace #t)]
            [(eq? message 'trace-off) (set! trace #f)]
            [else
             (error "make-register" "unknown request" message)]))
    dispatch))
(define (trace-on-reg machine register-name)
  ((get-register machine register-name) 'trace-on))
(define (trace-off-reg machine register-name)
  ((get-register machine register-name) 'trace-off))

;;; modified from 5.1
(define (make-instruction text)
  (list text '() #f))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cadr inst))
(define (instruction-breakpoint inst)
  (caddr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst (list proc (instruction-breakpoint inst))))
(define (set-instruction-breakpoint! inst bp)
  (set-cdr! (cdr inst) (list bp)))

#| Tests |#
(define (m515) (make-machine (list) '(
  label
  (assign a (const 1))
  (assign a (const 2))
  (assign a (const 3))
  done)))

(define-test (let ([m (m515)]) (begin
  (start m)
  (m 'instructions-executed)))
  '((assign a (const 1))
    (assign a (const 2))
    (assign a (const 3))))

(define-test (let ([m (m515)]) (begin
  (start m)
  (m 'instructions-executed-count)))
  3)
