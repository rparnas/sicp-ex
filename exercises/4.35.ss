#|

Exercise 4.35: Write a procedure "an-integer-between" that
returns an integer between two given bounds. This can be
used to implement a procedure that finds Pythagorean
triples, i.e., triples of integers (i, j, k) between the
given bounds such that i <= j and i^2 + j^2 = k^2, as
follows:

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

|#

(load-ex "4.23") ;;; skipping lazy evaulator of section 4.2

#| Code from book -- amb implementation |#
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define analyze-423 analyze)
(set! analyze (lambda (exp)
  (cond [(amb? exp) (analyze-amb exp)]
        [else (analyze-423 exp)])))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ([qval (text-of-quotation exp)])
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence (lambda-body exp))])
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs)
        (error "analyze-sequence" "empty sequence")
        (loop (car procs) (cdr procs)))))

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ([old-value (lookup-variable-value var env)])
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                          fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond [(primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail)]
        [(compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail)]
         [else
          (error "execute-application" "unknown procedure type" proc)]))

(define (analyze-amb exp)
  (let ([cprocs (map analyze (amb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                            (try-next (cdr choices))))))
      (try-next cprocs))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

#| Code from book -- amb interface |#

#| Answer |#

;;; updated to verify there is only one result.
(define (eval-one exp)
  (define (on-success value next)
    (let ([next-value (next)])
      (if (equal? next-value "fail")
          value
          (error "eval-one" "more than one possible value" next-value))))
  (define (on-fail) "fail")
  (ambeval exp (setup-environment) on-success on-fail))

;;; returns every possibility
(define (eval-all exp)
  (let ([results '()])
    (define (on-success value next)
      (set! results (append results (list value)))
      (next))
    (define (on-fail)
      results)
    (ambeval exp (setup-environment) on-success on-fail)))

;;; returns the first n possibilities
(define (eval-n exp n)
  (let ([results '()])
    (define (on-success value next)
      (set! results (append results (list value)))
      (set! n (- n 1))
      (if (= n 0)
          results
          (next)))
    (define (on-fail)
      results)
    (ambeval exp (setup-environment) on-success on-fail)))

;;; add some definitions to the environment
(define setup-environment-41 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-41)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))
    ;;; code from book
    (add '(define (require p) (if p (void) (amb))))
    (add '(define (an-integer-starting-from n)
            (amb n (an-integer-starting-from (+ n 1)))))
    ;;; answer
    (add '(define (an-integer-between low high)
            (if (> low high)
                (amb)
                (amb low (an-integer-between (+ low 1) high)))))
    ;;; code from book -- cleaned up
    (add '(define (a-pythagorean-triple-between low high)
            (let ((i (an-integer-between low high)))
              (let ((j (an-integer-between i high)))
                (let ((k (an-integer-between j high)))
                  (require (= (+ (* i i) (* j j)) (* k k)))
                  (list i j k))))))

    env))

#| Tests |#
(define-test (eval-all '(amb 1 2 3)) '(1 2 3))

(define-test (eval-all '(cons (amb 1 2 3) (amb 'a 'b 'c)))
             '((1 . a) (1 . b) (1 . c) 
               (2 . a) (2 . b) (2 . c) 
               (3 . a) (3 . b) (3 . c)))

(define-test (eval-n '(cons (amb 1 2 3) (amb 'a 'b 'c)) 2)
             '((1 . a) (1 . b)))

(define-test (eval-n '(an-integer-starting-from 1) 5)
             '(1 2 3 4 5))

(define-test (eval-all '(an-integer-between 1 10))
             '(1 2 3 4 5 6 7 8 9 10))

;;; this is pythogrean tripples, not primitive pythagorean triples
(define-test (eval-all '(a-pythagorean-triple-between 1 10))
             '((3 4 5) (6 8 10)))