#|

Exercise 5.40: Modify the compiler to maintain the
compile-time environment as described above. That is, add a
compile-time-environment argument to "compile" and the
various code generators, and extend it in
"compile-lambda-body".

|#

(load-ex "5.33")

#| Answer |#

;;; rewrite of 5.33
(define (comp exp target linkage cenv)
  (cond [(self-evaluating? exp)
         (compile-self-evaluating exp target linkage)]
        [(quoted? exp) (compile-quoted exp target linkage)]
        [(variable? exp)
         (compile-variable exp target linkage cenv)]
        [(assignment? exp)
         (compile-assignment exp target linkage cenv)]
        [(definition? exp)
         (compile-definition exp target linkage cenv)]
        [(if? exp) (compile-if exp target linkage cenv)]
        [(lambda? exp) (compile-lambda exp target linkage cenv)]
        [(begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           cenv)]
        [(cond? exp) (comp (cond->if exp) target linkage cenv)]
        [(is-let? exp) (comp (let->combination exp) target linkage cenv)] ; 5.43
        [(ocp? exp cenv) 
         (compile-ocp exp target linkage cenv)] ; 5.44
        [(application? exp)
         (compile-application exp target linkage cenv)]
        [else
         (error "compile" "unknown expression type" exp)]))

;;; rewrite from 5.33
;;; Could reference a global environment here.
(define (compile-variable exp target linkage cenv)
  (let ([lexical-address (find-variable exp cenv)])
    (end-with-linkage linkage
      (make-instruction-sequence '(env) (list target)
        (if (eq? lexical-address 'not-found)
            `((assign ,target (op lookup-variable-value) (const ,exp) (reg env)))
            `((assign ,target (op lexical-address-lookup) (const ,lexical-address) (reg env))))))))

;;; rewrite from 5.33
(define (compile-assignment exp target linkage cenv)
  (let* ([var (assignment-variable exp)]
         [lexical-address (find-variable exp cenv)]
         [get-value-code (comp (assignment-value exp) 'val 'next cenv)])
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
        (list (if (eq? lexical-address 'not-found)
                  `(perform (op set-variable-value!) (const ,var) (reg val) (reg env))
                  `(perform (op lexical-address-set!) (const ,lexical-address) (reg val) (reg env)))
              `(assign ,target (const ok))))))))

;;; rewrite from 5.33
(define (compile-definition exp target linkage cenv)
  (let ((var (definition-variable exp))
        (get-value-code
         (comp (definition-value exp) 'val 'next cenv)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!) (const ,var) (reg val) (reg env))
         (assign ,target (const ok))))))))

;;; rewrite from 5.33
(define (compile-if exp target linkage cenv)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (comp (if-predicate exp) 'val 'next cenv))
            (c-code
             (comp (if-consequent exp) target consequent-linkage cenv))
            (a-code
             (comp (if-alternative exp) target linkage cenv)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;;; rewrite from 5.33
(define (compile-sequence seq target linkage cenv)
  (if (last-exp? seq)
      (comp (first-exp seq) target linkage cenv)
      (preserving '(env continue)
       (comp (first-exp seq) target 'next cenv)
       (compile-sequence (rest-exps seq) target linkage cenv))))

;;; rewrite from 5.33
(define (compile-lambda exp target linkage cenv)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry cenv))
       after-lambda))))

;;; rewrite from 5.33
(define (compile-lambda-body exp proc-entry cenv)
  (let* ([formals (lambda-parameters exp)]
         [cenv (cons formals cenv)])
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp)) ; 5.43
                       'val
                       'return
                       cenv))))

;;; rewrite from 5.33
(define (compile-application exp target linkage cenv)
  (let ((proc-code (comp (operator exp) 'proc 'next cenv))
        (operand-codes
         (map (lambda (operand) (comp operand 'val 'next cenv))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

#| Answer -- 5.39 |#

(define (make-lexical-address skip-frames skip-vars)
  (cons skip-frames skip-vars))

(define (skip-frames lexical-address)
  (car lexical-address))

(define (skip-vars lexical-address)
  (cdr lexical-address))

(define (lexical-address-process proc lexical-address env)
  (define (scan-vals skip vals)
    (cond [(null? vals)
           (error "" "invalid lexical address")] ; impossible runtime error (?)
          [(= 0 skip)
           (proc vals)]
          [else
           (scan-vals (- skip 1) (cdr vals))]))
  (define (scan-frames skip env)
    (cond [(eq? env the-empty-environment)
           (error "" "invalid lexical adress")] ; impossible runtime error (?)
          [(= 0 skip)
           (scan-vals (skip-vars lexical-address)
                      (frame-values (first-frame env)))]
          [else
           (scan-frames (- skip 1) (enclosing-environment env))]))
  (scan-frames (skip-frames lexical-address) env))

(define (lexical-address-lookup lexical-address env)
  (lexical-address-process (lambda (vals) (car vals))
                           lexical-address 
                           env))

(define (lexical-address-set! lexical-address val env)
  (lexical-address-process (lambda (vals) (set-car! vals val)) 
                           lexical-address 
                           env))

#| Answer -- 5.41 |#
(define (find-variable var cenv)
  (define (get-var-index index vars)
    (cond [(null? vars) #f]
          [(eq? var (car vars)) index]
          [else (get-var-index (+ index 1) (cdr vars))]))
  (define (iter-frames frame-index cenv)
    (if (null? cenv)
        'not-found
        (let ([var-index (get-var-index 0 (car cenv))])
          (if var-index
              (make-lexical-address frame-index var-index)
              (iter-frames (+ frame-index 1) (cdr cenv))))))
  (iter-frames 0 cenv))

#| Answer -- 5.43 |#

;;; from 4.16
(define (make-assignment var value)
  (list 'set! var value))

;;; from 4.16
(define (scan-out-defines body)
  (define (iter lets result rest)
    (if (null? rest)
      (if (null? lets)
          body
          (let ([vars lets]
                [exps (map (lambda (x) ''*unassigned*) lets)]
                [body result])
            (list (make-let vars exps body))))
        (let ([first (car rest)])
          (if (definition? first)
            (let ([var (definition-variable first)]
                  [val (definition-value first)])
              (iter (append lets (list var))
                    (append result (list (make-assignment var val)))
                    (cdr rest)))
            (iter lets
                  (append result (list first))
                  (cdr rest))))))
  (iter '() '() body))

#| Answer -- 5.44 -- modified from 5.38 |#

(define open-coded-primitives '(= * - +))

(define (ocp? exp cenv)
  (if (not (pair? exp))
      #f
      (let* ([var (car exp)]
             [evar (find-variable var cenv)])
        (if (eq? evar 'not-found)
            (member (car exp) open-coded-primitives)
            #f))))

(define (compile-ocp exp target linkage cenv)
  (do-one (operator exp) (operands exp) target linkage cenv))

(define (do-one ocp operands target linkage cenv)
  (if (null? operands)
      (compile-ocp-call ocp '() target linkage) ; zero arguments
      (let ([one (comp (car operands) 'arg1 'next cenv)])
        (preserving '(continue env)
          one
          (do-two ocp (cdr operands) target linkage cenv)))))
(define (do-two ocp operands target linkage cenv)
  (if (null? operands)
      (compile-ocp-call ocp '(arg1) target linkage) ; one arguments
      (let ([two (comp (car operands) 'arg2 'next cenv)])
        (preserving '(arg1 continue env)
          two
          (do-n ocp (cdr operands) target linkage cenv)))))
(define (do-n ocp operands target linkage cenv)
  (if (null? operands)
      (compile-ocp-call ocp '(arg1 arg2) target linkage) ; final two arguments
      (let ([next (comp (car operands) 'arg2 'next cenv)])
        (preserving '(continue env)
          (compile-ocp-call ocp '(arg1 arg2) 'arg1 'next)
          (preserving '(arg1)
            next
            (do-n ocp (cdr operands) target linkage cenv))))))

;;; rewrite from 5.33
(define all-regs '(env proc val arg1 arg2 argl continue))

(define (compile-ocp-call ocp input-regs target linkage)
 (end-with-linkage linkage
  (make-instruction-sequence input-regs (list target)
    (list (append `(assign ,target (op ,ocp))
                  (map (lambda (r) (list 'reg r)) input-regs))))))

#| Tests -- infrastructure |#
(define (compile-test exp)
  (set! label-counter 0)
  (let* ([compiled-exp (comp exp 'val 'next '())]
         [m (make-machine 
              (list
                (list 'apply-primitive-procedure apply-primitive-procedure)
                (list 'cons cons)
                (list 'compound-procedure? compound-procedure?) ; added for 5.47
                (list 'compiled-procedure-env compiled-procedure-env)
                (list 'compiled-procedure-entry compiled-procedure-entry)
                (list 'define-variable! define-variable!)
                (list 'extend-environment extend-environment)
                (list 'false? false?)
                (list 'list list)
                (list 'lookup-variable-value lookup-variable-value)
                (list 'make-compiled-procedure make-compiled-procedure)
                (list 'primitive-procedure? primitive-procedure?)
                (list 'set-car! set-car!) ; added for 5.36
                (list 'set-cdr! set-cdr!) ; added for 5.36
                (list '= (lambda (a b) (if (= a b) 'true 'false))) ; added for 5.38
                (list '* *) ; added for 5.38
                (list '- -) ; added for 5.38
                (list '+ +) ; added for 5.38
                (list 'lexical-address-lookup lexical-address-lookup) ; added for 5.42
                (list 'lexical-address-set! lexical-address-set!) ; added for 5.42
                (list 'set-variable-value! set-variable-value!)) 
              (statements compiled-exp))])
    (set-register-contents! m 'env (setup-environment))
    (start m)
    ; (cons (get-register-contents m 'val) ((m 'stack) 'get-statistics))))
    (get-register-contents m 'val)))

(define (display-asm exp)
  (set! label-counter 0)
  (let ([compiled-exp (comp exp 'val 'next '())])
    (for-each (lambda (i)
                (newline)
                (if (not (symbol? i))
                    (display "  ")
                    (void))
                (display i))
              (statements (comp exp 'val 'next '())))))

#| Tests -- infrastructure |#

(define-test (find-variable 'c '((y z) (a b c d e) (x y))) '(1 . 2))
(define-test (find-variable 'x '((y z) (a b c d e) (x y))) '(2 . 0))
(define-test (find-variable 'w '((y z) (a b c d e) (x y))) 'not-found)

(define easy-prog
  '(begin
     (define (func) 0)
     (func)))

(define factorial-2-prog
  '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
    (factorial 2)))

(define factorial-5-prog
  '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
    (factorial 5)))

(define factorial-alt-2-prog
  '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))
    (factorial 2)))

(define factorial-alt-5-prog
  '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))
    (factorial 5)))

(define-test (compile-test easy-prog) 0)

(define-test (compile-test factorial-2-prog) 2)

(define-test (compile-test factorial-alt-2-prog) 2)

(define-test (compile-test factorial-5-prog) 120)

(define-test (compile-test factorial-alt-5-prog) 120)

#| Tests -- 5.39 |#

(define test-env 
  (extend-environment 
    '(a b c)
    '(a b c)
    (extend-environment
      '(d e f)
      '(d e f)
      (extend-environment
        '(g h i)
        '(g h i)
        the-empty-environment))))

; Disable tests to avoid exercise dependency confusion on subsequent exercises.
(define-test (lexical-address-lookup (make-lexical-address 0 0) test-env) 'a)
(define-test (lexical-address-lookup (make-lexical-address 1 1) test-env) 'e)
(define-test (lexical-address-lookup (make-lexical-address 2 2) test-env) 'i)
(define-test (begin
  (lexical-address-set! (make-lexical-address 1 2) 'x test-env)
  (lexical-address-lookup (make-lexical-address 1 2) test-env))
  'x)

#| Tests -- 5.42 |#
(define-test (compile-test
  '(((lambda (x y)
      (lambda (a b c d e)
        ((lambda (y z) (* x y z))
         (* a b x)
         (+ c d x))))
     3 4)
    1 2 3 4 5))
  180)

#| Tests -- 5.43 |#
(define-test (compile-test
  '((lambda ()
      (define x 2)
      (define y 3)
      (+ 2 3))))
  5)

#| Tests -- 5.40 |#

(define-test (compile-test
  '((lambda (+ * a b x y)
      (+ (* a x) (* b y)))
    - / 4 14 2 2))
  -5)
