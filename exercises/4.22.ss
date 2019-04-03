#|

Exercise 4.22: Extend the evaluator in this section to
support the special form "let". (See Exercise 4.6.)

|#

(load-ex "4.20")

#| Code from book -- self-evaluating |#
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

#| Code from book -- variable |#
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

#| Code from book -- quoted |#
(define (analyze-quoted exp)
  (let ([qval (text-of-quotation exp)])
    (lambda (env) qval)))

#| Code from book -- assignment |#
(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

#| Code from book -- definition |#
(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    (lambda (env)
      (define-variable! var (vproc env) env)
    'ok)))

#| Code from book -- if |#
(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

#| Code from book -- lambda |#
(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence (lambda-body exp))])
    (lambda (env) (make-procedure vars bproc env))))

#| Code from book -- application |#
(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env))
             aprocs)))))

(define (execute-application proc args)
  (cond [(primitive-procedure? proc)
         (apply-primitive-procedure proc args)]
        [(compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc)))]
        [else
          (error "execute-application" "unknown proceture type" proc)]))

#| Code from book -- utilities |#
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs) (error "analyze" "empty sequence"))
        (loop (car procs) (cdr procs))))


(define (eval exp env) ((analyze exp) env))

#| Answer -- also fixed all other syntaxes |#
(define (analyze exp)
  (cond 
        ;;; 4.4 and, or
        [(is-and? exp) (analyze (and-transform exp))]
        [(is-or? exp) (analyze (or-transform exp))]
        ;;; 4.6 let
        [(is-let? exp) (analyze (let->combination exp))]
        ;;; 4.7 let*
        [(is-let*? exp) (analyze (let*->nested-lets exp))]
        ;;; 4.9 do-while, for
        [(is-do-while? exp) (analyze (transform-do-while exp))]
        [(is-for? exp) (analyze (transform-for exp))]
        ;;; 4.20 letrec
        [(is-letrec? exp) (analyze (letrec->transform exp))]
        ;;; code from book
        [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(application? exp) (analyze-application exp)]
        [else (error "analyze" "unknown expression type" exp)]))

;;; 4.16 undo the changes that added scan-out-defines
(define (procedure-body p) 
  (caddr p))

#| Tests -- see regression tests |#
