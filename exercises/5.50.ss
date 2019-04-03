#|

Exercise 5.50: Use the compiler to compile the metacircular
evaluator of Section 4.1 and run this program using the
register-machine simulator. (To compile more than one
definition at a time, you can package the definitions in a
"begin".) The resulting interpreter will run very slowly
because of the multiple levels of interpretation, but
getting all the details to work is an instructive exercise.

|#

(load-ex "5.46") ; use the best version of the compiler, mainly in 5.40

#| Answer |#
(append! primitive-procedures
  (list (list 'number? number?)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)))

(define (me-on-rms exp)
  (compile-test `(begin

#| Hacks |#

; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (cadr x) (car (cdr x)))

#| copy-paste from 4.1 |#

#| Code from book -- hacks |#
;(define apply-in-underlying-scheme apply)
;(define (eval) (error "eval" "not implemented"))
;(define (apply) (error "apply" "not implemented"))

#| Code from book -- self-evaluating |#
(define (self-evaluating? exp)
  (cond [(number? exp) 'true] ; modified
        [(string? exp) 'true] ; modified
        [else 'false])) ; modified

#| Code from book -- variable |#
(define (variable? exp) (symbol? exp))

#| Code from book -- quoted |#
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

#| Code from book -- assignment |#
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

#| Code from book -- definition |#
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

#| Code from book -- if |#
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cddr exp)))
      (cadddr exp)
      'false))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

#| Code from book -- lambda |#
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

#| Code from book -- begin |#
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))
(define (make-begin seq) (cons 'begin seq))

#| Code from book -- cond |#
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "cond" "else clause isn't last" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

#| Code from book -- application |#
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

#| Code from book -- utilities |#
(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      'false)) ; modified

#| Code from book -- eval and apply |#
(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)]
        [(begin? exp)
         (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else
         (error "eval" "unknown expression type" exp)]))

(define (apply procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure)))]
        [else
         (error "apply" "unknown procedure type" procedure)]))

(define (true? x)
  (not (eq? x 'false)))

(define (false? x)
  (eq? x 'false))

#| code from book -- environments |#

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "extend-environment" "too many arguments supplied" vars vals)
          (error "extend-environment" "too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (car vals)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "" "unbound-variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "set!" "unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars)
             (add-binding-to-frame! var val frame)]
            [(eq? var (car vars))
             (set-car! vals var)]
            [else (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))

#| code from book -- primitive procedures |#
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'display display)
        (list 'format format)
        (list 'list list)
        (list 'append append)
        (list 'equal? (lambda (a b) (if (equal? a b) 'true 'false)))
        (list 'eq? (lambda (a b) (if (eq? a b) 'true 'false)))
        (list 'integer? (lambda (x) (if (integer? x) 'true 'false)))
        (list 'newline newline)
        (list 'not (lambda (x) (if (true? x) 'false 'true)))
        (list 'length length)
        (list 'member (lambda (obj list) (if (member obj list) 'true 'false)))
        (list 'memq (lambda (obj list) (if (memq obj list) 'true 'false)))
        (list 'null? (lambda (x) (if (null? x) 'true 'false)))
        (list 'remainder remainder)
        (list 'sqrt sqrt)
        (list 'abs abs)
        (list 'void void)
        (list 'even? (lambda (x) (if (even? x) 'true 'false)))
        (list 'odd? (lambda (x) (if (odd? x) 'true 'false)))
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> (lambda (a b) (if (> a b) 'true 'false)))
        (list '>= (lambda (a b) (if (>= a b) 'true 'false)))
        (list '< (lambda (a b) (if (< a b) 'true 'false)))
        (list '<= (lambda (a b) (if (<= a b) 'true 'false)))
        (list '= (lambda (a b) (if (= a b) 'true 'false)))))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

; just use this as a register machine primitive
;(define (apply-primitive-procedure proc args)
;  (apply-in-underlying-scheme
;    (primitive-implementation proc) args))

#| code from book -- repl |#
(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true 'true initial-env)
    (define-variable! 'false 'false initial-env)
    initial-env))

;;;;;; run the expression
(define (eval-one exp)
  (eval exp (setup-environment)))
(eval-one ,exp)

)))

#| Tests -- from 4.1 |#
(define-test (me-on-rms 5) 5)
(define-test (me-on-rms "abc") "abc")
(define-test (me-on-rms ''(+ 2 2)) '(+ 2 2))
(define-test (me-on-rms '(begin (define x 5)
                                (set! x 6)
                                x))
             6)
(define-test (me-on-rms '(begin (define x 5) x)) 5)
(define-test (me-on-rms '(begin (define (x) 5) (x))) 5)
(define-test (me-on-rms '(if 0 1 2)) 1)
(define-test (me-on-rms '(if true 1 2)) 1)
(define-test (me-on-rms '(if false 1 2)) 2)
(define-test (me-on-rms '(begin
                           (define plus-one (lambda (x) (+ x 1)))
                           (plus-one 1)))
              2)
(define-test (me-on-rms '(begin 1 2 3 4)) 4)
(define-test (me-on-rms '(cond [else 0])) 0)
(define-test (me-on-rms '(cond [true 0] [false 1] [else 2])) 0)
(define-test (me-on-rms '(cond [false 0] [true 1] [else 2])) 1)
(define-test (me-on-rms '(cond [false 0] [false 1] [else 2])) 2)
(define-test (me-on-rms '(+ 2 2)) 4)
