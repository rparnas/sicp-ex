#|

Exercise 4.78: Redesign the query language as a
nondeterministic program to be implemented using the
evaluator of Section 4.3, rather than as a stream process.
In this approach, each query will produce a single answer
(rather than the stream of all answers) and the user can
type "try-again" to see more answers. You should find that
much of the mechanism we built in this section is subsumed
by nondeterministic search and backtracking. You will
probably also find, however, that your new query language
has subtle differences in behavior from the one implemented
here. Can you find examples that illustrate this difference?

|#

(load-ex "4.35") ;;; earliest version of amb system

#| Answer

The original query system interleaves results from disjoins whereas this system
does not. It would be nice to implement some sort of amb interleave syntax. A
naive implementation might be something like forking the amb evaluator into
multiple copies and switching back and forth asking for ambiguous values.

All tests from 4.55 to 4.58 pass.

A better implementation for negate is probably to implement new syntax that
allows require statements that pass only if a given expression has no valid
ambiguous values. However, I implemented recursively invoking a new ambiguous
evaluator instead just to try it.

|#

;;; add to primitive procedures
(set! primitive-procedures (append primitive-procedures (list
  (list 'assoc (lambda (obj alist)
                 (let ([result (assoc obj alist)])
                   (if result
                       result
                       'false))))
  (list 'caddr (lambda (x) (car (cdr (cdr x)))))
  (list 'cddr (lambda (x) (cdr (cdr x))))
  (list 'pair? (lambda (x) (if (pair? x) 'true 'false)))
  (list 'symbol? (lambda (x) (if (symbol? x) 'true 'false)))
  (list 'symbol->string symbol->string)
  (list 'number? (lambda (x) (if (number? x) 'true 'false)))
  (list 'string-append string-append)
  (list 'number->string number->string)
  (list 'string->symbol string->symbol)
  (list 'string-length string-length)
  (list 'string=? (lambda (a b) (if (string=? a b) 'true 'false)))
  (list 'substring substring)
  (list 'error error)
  (list 'reverse reverse)
  (list 'eval-all eval-all)
  (list 'eval-one eval-one))))

;;; added modified implementation from 4.55 only.
(define setup-environment-435 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-435)])
    (define (add exp) (ambeval exp env (lambda (value next) (void)) (lambda () (error "setup-environment" "fail!"))))
    #| New code |#
    (add '(define (amb-list ls)
            (if (null? ls)
                (amb)
                (amb (car ls)
                     (amb-list (cdr ls))))))
    (add '(define (cadr x) (car (cdr x))))
    (add '(define (for-each proc ls)
            (if (null? ls)
                (void)
                (begin
                  (proc (car ls))
                  (for-each proc (cdr ls))))))
    (add '(define (map proc ls)
            (if (null? ls)
                ls
                (cons (proc (car ls)) (map proc (cdr ls))))))
    (add '(define (find proc ls)
            (cond [(null? ls) 'false]
                  [(proc (car ls)) (car ls)]
                  [else (find proc (cdr ls))])))
    #| Code from 2.73 |#
    (add '(define op-table (list)))
    (add '(define (put op type proc) 
            (set! op-table (cons (list op type proc) op-table))))
    (add '(define (get op type)
            (let ([ret (find (lambda (x) 
                               (and (equal? op (car x))
                                    (equal? type (cadr x))))
                             op-table)])
              (if ret
                  (caddr ret)
                  ret))))
    #| Code from 4.1 |#
    (add '(define (tagged-list? exp tag)
            (if (pair? exp)
                (eq? (car exp) tag)
                false)))
    #| Code from book -- 4.4.4.1 The Driver Loop and Instantiation |#
    (add '(define (instantiate exp frame unbound-var-handler)
            (define (copy exp)
              (cond [(var? exp)
                     (let ([binding (binding-in-frame exp frame)])
                       (if binding
                           (copy (binding-value binding))
                           (unbound-var-handler exp frame)))]
                    [(pair? exp)
                     (cons (copy (car exp))
                           (copy (cdr exp)))]
                    [else exp]))
            (copy exp)))
    #| Code from book --- 4.4.4.2 The Evaluator |#
    (add '(define (qeval query frame)
            (let ([qproc (get (type query) 'qeval)])
              (if qproc
                  (qproc (contents query) frame)
                  (simple-query query frame)))))
    (add '(define (simple-query query-pattern frame)                            ; modified
            (amb (find-assertions query-pattern frame)
                 (apply-rules query-pattern frame))))
    (add '(define (conjoin conjuncts frame)                                     ; modified
      (if (empty-conjunction? conjuncts)
          frame
          (conjoin (rest-conjuncts conjuncts)
                   (qeval (first-conjunct conjuncts) frame)))))
    (add '(put 'and 'qeval conjoin))
    (add '(define (disjoin disjuncts frame)                                     ; modified
            (require (not (empty-disjunction? disjuncts)))
            (amb (qeval (first-disjunct disjuncts) frame)
                 (disjoin (rest-disjuncts disjuncts) frame))))
    (add '(put 'or 'qeval disjoin))
    (add '(define (negate operands frame)
            (let* ([neg (negated-query operands)] 
                   [inner-results 
                     (eval-all (list 'do-query-full
                                     (list 'quote do-query-db)
                                     (list 'quote neg)  
                                     (list 'quote frame)))])
              (require (null? inner-results))
              frame)))    
    (add '(put 'not 'qeval negate))
    (add '(define (lisp-value call frame)                                       ; modified
            (require (execute
                      (instantiate
                       call
                       frame
                       (lambda (v f) (error "lisp-value" "unknown pat var" v)))))
            frame))
    (add '(put 'lisp-value 'qeval lisp-value))
    (add '(define (execute exp)                                                 ; modified                                                    
            (eval-one exp)))
    (add '(define (always-true ignore frame) frame))
    (add '(put 'always-true 'qeval always-true))

    #| Code from book -- 4.4.4.3 Finding Assertions by Pattern Matching |#
    (add '(define (find-assertions pattern frame)                               ; modified
      (let ([datum (fetch-assertions pattern frame)])
        (check-an-assertion datum pattern frame))))
    (add '(define (check-an-assertion assertion query-pat query-frame)          ; modified
      (let ([match-result (pattern-match query-pat assertion query-frame)])
        (require (not (equal? match-result 'failed)))
        match-result)))
    (add '(define (pattern-match pat dat frame)
      (cond [(eq? frame 'failed) 'failed]
            [(equal? pat dat) frame]
            [(var? pat) (extend-if-consistent pat dat frame)]
            [(and (pair? pat) (pair? dat))
             (pattern-match (cdr pat)
                            (cdr dat)
                            (pattern-match (car pat)
                                           (car dat)
                                           frame))]
            [else 'failed])))
    (add '(define (extend-if-consistent var dat frame)
      (let ([binding (binding-in-frame var frame)])
        (if binding
            (pattern-match (binding-value binding) dat frame)
            (extend var dat frame)))))

    #| Code from book -- 4.4.4.4 Rules and Unification |#
    (add '(define (apply-rules pattern frame)                                   ; modified
            (let ([rule (fetch-rules pattern frame)])
              (apply-a-rule rule pattern frame))))
    (add '(define (apply-a-rule rule query-pattern query-frame)                 ; modified
            (let ([clean-rule (rename-variables-in rule)])
              (let ([unify-result (unify-match query-pattern (conclusion clean-rule) query-frame)])
                (if (eq? unify-result 'failed)
                    (amb)
                    (qeval (rule-body clean-rule) unify-result))))))            
    (add '(define (rename-variables-in rule)
            (let ([rule-application-id (new-rule-application-id)])
              (define (tree-walk exp)
                (cond [(var? exp)
                       (make-new-variable exp rule-application-id)]
                      [(pair? exp)
                       (cons (tree-walk (car exp))
                             (tree-walk (cdr exp)))]
                      [else exp]))
              (tree-walk rule))))
    (add '(define (unify-match p1 p2 frame)
            (cond [(eq? frame 'failed) 'failed]
                  [(equal? p1 p2) frame]
                  [(var? p1) (extend-if-possible p1 p2 frame)]
                  [(var? p2) (extend-if-possible p2 p1 frame)]
                  [(and (pair? p1) (pair? p2))
                   (unify-match (cdr p1)
                                (cdr p2)
                                (unify-match (car p1)
                                             (car p2)
                                             frame))]
                  (else 'failed))))
    (add '(define (extend-if-possible var val frame)
            (let ([binding (binding-in-frame var frame)])
              (cond [binding
                     (unify-match (binding-value binding) val frame)]
                    [(var? val)
                     (let ((binding (binding-in-frame val frame)))
                       (if binding
                           (unify-match var (binding-value binding) frame)
                           (extend var val frame)))]
                    [(depends-on? val var frame)
                     'failed]
                    [else (extend var val frame)]))))
    (add '(define (depends-on? exp var frame)
            (define (tree-walk e)
              (cond [(var? e)
                     (if (equal? var e)
                         'true
                     (let ((b (binding-in-frame e frame)))
                       (if b
                           (tree-walk (binding-value b))
                           'false)))]
                    [(pair? e)
                     (or (tree-walk (car e))
                         (tree-walk (cdr e)))]
                    [else 'false]))
            (tree-walk exp)))

    #| Code from book -- 4.4.4.5 Maintaining the Data Base |#
    (add '(define THE-ASSERTIONS '()))                                          ; modified
    (add '(define (fetch-assertions pattern frame)
            (if (use-index? pattern)
                (get-indexed-assertions pattern)
                (get-all-assertions))))
    (add '(define (get-all-assertions) (amb-list THE-ASSERTIONS)))              ; modified
    (add '(define (get-indexed-assertions pattern)
            (amb-list (get-stream (index-key-of pattern) 'assertion-stream))))
    (add '(define (add-assertion! assertion)                                    ; modified
            (store-assertion-in-index assertion)
            (let ([old-assertions THE-ASSERTIONS])
              (set! THE-ASSERTIONS
                    (cons assertion old-assertions))
              'ok)))
    (add '(define (store-assertion-in-index assertion)                          ; modified
            (if (indexable? assertion)
                (let ([key (index-key-of assertion)])
                  (let ([current-assertion-stream
                         (get-stream key 'assertion-stream)])
                    (put key
                         'assertion-stream
                         (cons assertion
                               current-assertion-stream))))
                (void))))
    (add '(define THE-RULES '()))                                               ; modified
    (add '(define (fetch-rules pattern frame)
            (if (use-index? pattern)
                (get-indexed-rules pattern)
                (get-all-rules))))
    (add '(define (get-all-rules) (amb-list THE-RULES)))                        ; modified
    (add '(define (get-indexed-rules pattern)                                   ; modified
            (amb
              (amb-list (get-stream (index-key-of pattern) 'rule-stream))
              (amb-list (get-stream '? 'rule-stream)))))
    (add '(define (add-rule! rule)                                              ; modified
            (store-rule-in-index rule)
            (let ([old-rules THE-RULES])
              (set! THE-RULES (cons rule old-rules))
              'ok)))
    (add '(define (store-rule-in-index rule)                                    ; modified
            (let ([pattern (conclusion rule)])
              (if (indexable? pattern)
                  (let ([key (index-key-of pattern)])
                    (let ([current-rule-stream
                           (get-stream key 'rule-stream)])
                      (put key
                           'rule-stream
                           (cons rule current-rule-stream))))
                  (void)))))
    (add '(define (get-stream key1 key2)                                        ; modified
            (let ([s (get key1 key2)])
              (if s s '()))))
    (add '(define (add-rule-or-assertion! assertion)
            (if (rule? assertion)
                (add-rule! assertion)
                (add-assertion! assertion))))
    (add '(define (indexable? pat)
            (or (constant-symbol? (car pat))
                (var? (car pat)))))
    (add '(define (index-key-of pat)
            (let ([key (car pat)])
              (if (var? key) '? key))))
    (add '(define (use-index? pat)
            (constant-symbol? (car pat))))

    #| Code from book -- 4.4.4.7 Query Syntax Procedures |#
    (add '(define (type exp)
            (if (pair? exp)
                (car exp)
                (error "type" "unknown expression" exp))))
    (add '(define (contents exp)
            (if (pair? exp)
                (cdr exp)
                (error "contents" "unknown expression" exp))))
    (add '(define (assertion-to-be-added? exp) (eq? (type exp) 'assert!)))
    (add '(define (add-assertion-body exp) (car (contents exp))))
    (add '(define (empty-conjunction? exps) (null? exps)))
    (add '(define (first-conjunct exps) (car exps)))
    (add '(define (rest-conjuncts exps) (cdr exps)))
    (add '(define (empty-disjunction? exps) (null? exps)))
    (add '(define (first-disjunct exps) (car exps)))
    (add '(define (rest-disjuncts exps) (cdr exps)))
    (add '(define (negated-query exps) (car exps)))
    (add '(define (pred exps) (car exps)))
    (add '(define (args exps) (cdr exps)))
    (add '(define (rule? statement) (tagged-list? statement 'rule)))
    (add '(define (conclusion rule) (cadr rule)))
    (add '(define (rule-body rule)
      (if (null? (cddr rule)) '(always-true) (caddr rule))))
    (add '(define (query-syntax-process exp)
            (map-over-symbols expand-question-mark exp)))
    (add '(define (map-over-symbols proc exp)
            (cond [(pair? exp)
                   (cons (map-over-symbols proc (car exp))
                         (map-over-symbols proc (cdr exp)))]
                  [(symbol? exp) (proc exp)]
                  [else exp])))
    (add '(define (expand-question-mark symbol)
            (let ([chars (symbol->string symbol)])
              (if (string=? (substring chars 0 1) "?")
                  (list '?
                        (string->symbol
                          (substring chars 1 (string-length chars))))
                  symbol))))
    (add '(define (var? exp) (tagged-list? exp '?)))
    (add '(define (constant-symbol? exp) (symbol? exp)))
    (add '(define rule-counter 0))
    (add '(define (new-rule-application-id)
      (set! rule-counter (+ 1 rule-counter))
      rule-counter))
    (add '(define (make-new-variable var rule-application-id)
            (cons '? (cons rule-application-id (cdr var)))))
    (add '(define (contract-question-mark variable)
            (string->symbol
              (string-append "?"
              (if (number? (cadr variable))
                  (string-append (symbol->string (caddr variable))
                                 "-"
                                 (number->string (cadr variable)))
              (symbol->string (cadr variable)))))))
    (add '(define (make-binding variable value) (cons variable value)))
    (add '(define (binding-variable binding) (car binding)))
    (add '(define (binding-value binding) (cdr binding)))
    (add '(define (binding-in-frame variable frame) (assoc variable frame)))
    (add '(define (extend variable value frame) (cons (make-binding variable value) frame)))

    #| Tests -- Infrastructure |#
    (add '(define do-query-db 0))
    (add '(define (do-query db query) (do-query-full db (query-syntax-process query) '())))
    (add '(define (do-query-full db q init-frame)
            (set! do-query-db db)                               
            (for-each add-rule-or-assertion! (map query-syntax-process (reverse db)))
            (let* ([frame (qeval q init-frame)])
              (instantiate q frame (lambda (v f) (contract-question-mark v))))))
    (add '(define inf-db '(
            (ones ())
            (twos ())
            (rule (ones (1 . ?x))
                  (ones ?x))
            (rule (twos (2 . ?x))
                  (twos ?x)))))
    (add '(define test-db '(
      #| 4.55 |#
      ;;; Ben Bitdiddle
      (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
      (job (Bitdiddle Ben) (computer wizard))
      (salary (Bitdiddle Ben) 60000)
      (supervisor (Bitdiddle Ben) (Warbucks Oliver))
      ;;; Alyssa P Hacker
      (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
      (job (Hacker Alyssa P) (computer programmer))
      (salary (Hacker Alyssa P) 40000)
      (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
      ;;; Cy D Fect
      (address (Fect Cy D) (Cambridge (Ames Street) 3))
      (job (Fect Cy D) (computer programmer))
      (salary (Fect Cy D) 35000)
      (supervisor (Fect Cy D) (Bitdiddle Ben))
      ;;; Lem E Tweakit
      (address (Tweakit Lem E) (Boston (Bay State Road) 22))
      (job (Tweakit Lem E) (computer technician))
      (salary (Tweakit Lem E) 25000)
      (supervisor (Tweakit Lem E) (Bitdiddle Ben))
      ;;; Louis Reasoner
      (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
      (job (Reasoner Louis) (computer programmer trainee))
      (salary (Reasoner Louis) 30000)
      (supervisor (Reasoner Louis) (Hacker Alyssa P))
      ;;; Warbucks Oliver
      (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
      (job (Warbucks Oliver) (administration big wheel))
      (salary (Warbucks Oliver) 150000)
      ;;; Eben Scrooge
      (address (Scrooge Eben) (Weston (Shady Lane) 10))
      (job (Scrooge Eben) (accounting chief accountant))
      (salary (Scrooge Eben) 75000)
      (supervisor (Scrooge Eben) (Warbucks Oliver))
      ;;; Robert Cratchet
      (address (Cratchet Robert) (Allston (N Harvard Street) 16))
      (job (Cratchet Robert) (accounting scrivener))
      (salary (Cratchet Robert) 18000)
      (supervisor (Cratchet Robert) (Scrooge Eben))
      ;;; Aull Dewitt
      (address (Aull DeWitt) (Slumerville (Onion Square) 5))
      (job (Aull DeWitt) (administration secretary))
      (salary (Aull DeWitt) 25000)
      (supervisor (Aull DeWitt) (Warbucks Oliver))
      ;;; Assertions
      (can-do-job (computer wizard) (computer programmer))
      (can-do-job (computer wizard) (computer technician))
      (can-do-job (computer programmer) (computer programmer trainee))
      (can-do-job (administration secretary) (administration big wheel))
      ;;; Rules
      (rule (lives-near ?person-1 ?person-2)
            (and (address ?person-1 (?town . ?rest-1))
                 (address ?person-2 (?town . ?rest-2))
                 (not (same ?person-1 ?person-2))))
      (rule (same ?x ?x))
      (rule (wheel ?person)
        (and (supervisor ?middle-manager ?person)
             (supervisor ?x ?middle-manager)))
      (rule (outranked-by ?staff-person ?boss)
        (or (supervisor ?staff-person ?boss)
            (and (supervisor ?staff-person ?middle-manager)
                 (outranked-by ?middle-manager ?boss))))
      #| 4.57 |#
      (rule (can-replace-job ?job1 ?job2)
              (or (same ?job1 job2)
                  (can-do-job ?job1 ?job2)
                  (and (can-do-job ?job1 ?middle-job)
                       (can-replace-job ?middle-job ?job2))))
      (rule (can-replace ?p1 ?p2)
            (and (job ?p1 ?job1)
                 (job ?p2 ?job2)
                 (not (same ?p1 ?p2))
                 (or (same ?job1 ?job2)
                     (can-replace-job ?job1 ?job2))))
      #| 4.58 |#
      (rule (is-big-shot ?person ?division)
        (and (job ?person (?division . ?any0) . ?any1)
             (or (not (supervisor ?person ?super))
                 (and (supervisor ?person ?super)
                      (not (job ?super (?division . ?any2) . ?any3))))))
      )))
    env))

#| Tests -- infrastructure |#
(define test-db 'test-db)
(define inf-db 'inf-db)

;;; hack to avoid rewriting tests
(define (do-query db q)
  (eval-all `(do-query ,db ',q)))
(define (do-query-n db q n)
  (eval-n `(do-query ,db ',q) n))

;;; addition test for amb system
(define-test (eval-all '(let ([inner-results (eval-all '(amb 1 2 3))])
                          inner-results))
             '((1 2 3)))

#| Tests -- 4.55 |#
(define-test (do-query test-db '(job ?x (computer programmer)))
             '((job (Hacker Alyssa P) (computer programmer))
               (job (Fect Cy D) (computer programmer))))
(define-test (do-query test-db '(address ?x ?y))
  '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)) 
    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (address (Aull DeWitt) (Slumerville (Onion Square) 5))))
(define-test (do-query test-db '(supervisor ?x ?x)) '())
(define-test (do-query test-db '(job ?x (computer ?type)))
  '((job (Bitdiddle Ben) (computer wizard))
   (job (Hacker Alyssa P) (computer programmer))
   (job (Fect Cy D) (computer programmer))
   (job (Tweakit Lem E) (computer technician))))
(define-test (do-query test-db '(job ?x (computer . ?type)))
  '((job (Bitdiddle Ben) (computer wizard))
    (job (Hacker Alyssa P) (computer programmer))
    (job (Fect Cy D) (computer programmer))
    (job (Tweakit Lem E) (computer technician))
    (job (Reasoner Louis) (computer programmer trainee))))
(define-test (do-query test-db 
  '(and (job ?person (computer programmer))
        (address ?person ?where)))
  '((and (job (Hacker Alyssa P) (computer programmer))
         (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
    (and (job (Fect Cy D) (computer programmer))
         (address (Fect Cy D) (Cambridge (Ames Street) 3)))))
(define-test (do-query test-db                                                  ; different order
  '(or (supervisor ?x (Bitdiddle Ben))             
       (supervisor ?x (Hacker Alyssa P))))
  '((or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
        (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
    (or (supervisor (Fect Cy D) (Bitdiddle Ben))
        (supervisor (Fect Cy D) (Hacker Alyssa P)))
    (or (supervisor (Tweakit Lem E) (Bitdiddle Ben))
        (supervisor (Tweakit Lem E) (Hacker Alyssa P)))
    (or (supervisor (Reasoner Louis) (Bitdiddle Ben))
        (supervisor (Reasoner Louis) (Hacker Alyssa P)))))
(define-test (do-query test-db 
  '(and (job ?x . ?details) (not (job ?x (computer programmer)))))
  '((and (job (Bitdiddle Ben) (computer wizard))
         (not (job (Bitdiddle Ben) (computer programmer))))
    (and (job (Tweakit Lem E) (computer technician))
         (not (job (Tweakit Lem E) (computer programmer))))
    (and (job (Reasoner Louis) (computer programmer trainee))
         (not (job (Reasoner Louis) (computer programmer))))
    (and (job (Warbucks Oliver) (administration big wheel))
         (not (job (Warbucks Oliver) (computer programmer))))
    (and (job (Scrooge Eben) (accounting chief accountant))
         (not (job (Scrooge Eben) (computer programmer))))
    (and (job (Cratchet Robert) (accounting scrivener))
         (not (job (Cratchet Robert) (computer programmer))))
    (and (job (Aull DeWitt) (administration secretary))
         (not (job (Aull DeWitt) (computer programmer))))))
(define-test (do-query test-db 
  '(and (supervisor ?x (Bitdiddle Ben)) (not (job ?x (computer programmer)))))
  '((and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
          (not (job (Tweakit Lem E) (computer programmer))))))
(define-test (do-query test-db 
  '(and (salary ?person ?amount) (lisp-value > ?amount 30000)))
  '((and (salary (Bitdiddle Ben) 60000)
         (lisp-value > 60000 30000))
    (and (salary (Hacker Alyssa P) 40000)
         (lisp-value > 40000 30000))
    (and (salary (Fect Cy D) 35000) 
         (lisp-value > 35000 30000))
    (and (salary (Warbucks Oliver) 150000)
         (lisp-value > 150000 30000))
    (and (salary (Scrooge Eben) 75000)
         (lisp-value > 75000 30000))))
(define-test (do-query test-db 
  '(lives-near ?x (Bitdiddle Ben)))
  '((lives-near (Reasoner Louis) (Bitdiddle Ben))
   (lives-near (Aull DeWitt) (Bitdiddle Ben))))
(define-test (do-query test-db 
  '(and (job ?x (computer programmer))
                                     (lives-near ?x (Bitdiddle Ben))))
  '())
(define-test (do-query-n inf-db 
  '(ones ?x) 5)
  '((ones ())
    (ones (1))
    (ones (1 1))
    (ones (1 1 1))
    (ones (1 1 1 1))))
(define-test (do-query test-db 
  '(supervisor ?supervisee (Bitdiddle Ben)))
  '((supervisor (Hacker Alyssa P) (Bitdiddle Ben))
    (supervisor (Fect Cy D) (Bitdiddle Ben))
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))))
(define-test (do-query test-db 
  '(job ?name (accounting . ?details)))
  '((job (Scrooge Eben) (accounting chief accountant))
    (job (Cratchet Robert) (accounting scrivener))))
(define-test (do-query test-db 
  '(address ?name (Slumerville . ?details)))
  '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (address (Aull DeWitt) (Slumerville (Onion Square) 5))))

#| 4.56 Tests |#
(define-test (do-query test-db 
  '(and (supervisor ?x (Bitdiddle Ben)) (address ?x . ?details)))
  '((and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
      (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
    (and (supervisor (Fect Cy D) (Bitdiddle Ben))
         (address (Fect Cy D) (Cambridge (Ames Street) 3)))
    (and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
         (address (Tweakit Lem E) (Boston (Bay State Road) 22)))))
(define-test (do-query test-db
  '(and (salary (Bitdiddle Ben) ?bamount)
        (salary ?person ?pamount)
        (lisp-value < ?pamount ?bamount)))
  '((and (salary (Bitdiddle Ben) 60000)
         (salary (Hacker Alyssa P) 40000)
         (lisp-value < 40000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Fect Cy D) 35000)
         (lisp-value < 35000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Tweakit Lem E) 25000)
         (lisp-value < 25000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Reasoner Louis) 30000)
         (lisp-value < 30000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Cratchet Robert) 18000)
         (lisp-value < 18000 60000))
    (and (salary (Bitdiddle Ben) 60000)
         (salary (Aull DeWitt) 25000)
         (lisp-value < 25000 60000))))
(define-test (do-query test-db
  '(and (supervisor ?person ?super)
        (job ?super . ?any0)
        (not (job ?super (computer . ?any1)))))
  '((and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
         (job (Warbucks Oliver) (administration big wheel))
         (not (job (Warbucks Oliver) (computer . ?any1))))
    (and (supervisor (Scrooge Eben) (Warbucks Oliver))
         (job (Warbucks Oliver) (administration big wheel))
         (not (job (Warbucks Oliver) (computer . ?any1))))
    (and (supervisor (Cratchet Robert) (Scrooge Eben))
         (job (Scrooge Eben) (accounting chief accountant))
         (not (job (Scrooge Eben) (computer . ?any1))))
    (and (supervisor (Aull DeWitt) (Warbucks Oliver))
         (job (Warbucks Oliver) (administration big wheel))
         (not (job (Warbucks Oliver) (computer . ?any1))))))

#| 4.57 |#
(define-test (do-query test-db                                                  ; different order
 '(can-replace-job ?j1 ?j2))
 '((can-replace-job job2 ?job2-1)
   (can-replace-job (computer wizard) (computer programmer))
   (can-replace-job (computer wizard) (computer technician))
   (can-replace-job (computer programmer) (computer programmer trainee))
   (can-replace-job (administration secretary) (administration big wheel))
   (can-replace-job (computer wizard) (computer programmer trainee))))
(define-test (do-query test-db                                                  ; different order
  '(can-replace ?p1 ?p2))
  '((can-replace (Bitdiddle Ben) (Hacker Alyssa P)) 
    (can-replace (Bitdiddle Ben) (Fect Cy D))
    (can-replace (Bitdiddle Ben) (Tweakit Lem E))
    (can-replace (Bitdiddle Ben) (Reasoner Louis))
    (can-replace (Hacker Alyssa P) (Fect Cy D))
    (can-replace (Hacker Alyssa P) (Reasoner Louis))
    (can-replace (Fect Cy D) (Hacker Alyssa P))
    (can-replace (Fect Cy D) (Reasoner Louis))
    (can-replace (Aull DeWitt) (Warbucks Oliver))))

#| 4.58 |#
(define-test (do-query test-db                                                  ; different order
  '(is-big-shot ?x ?y))
  '((is-big-shot (Bitdiddle Ben) computer)
    (is-big-shot (Warbucks Oliver) administration)
    (is-big-shot (Scrooge Eben) accounting)))

