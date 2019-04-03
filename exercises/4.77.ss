#|

Exercise 4.77: In Section 4.4.3 we saw that "not" and
"lisp-value" can cause the query language to give "wrong"
answers if these filtering operations are applied to frames
in which variables are unbound. Devise a way to fix this
shortcoming. One idea is to perform the filtering in a
"delayed" manner by appending to the frame a "promise" to
filter that is fulfilled only when enough variables have
been bound to make the operation possible. We could wait to
perform filtering until all other operations have been
performed. However, for efficiency's sake, we would like to
perform filtering as soon as possible so as to cut down on
the number of intermediate frames generated.

|#

(load-ex "4.66") ; without loop detection
; (load-ex "4.75")

#| Answer 

My implementation considers a frame to be bindings and promises. All promises in
the frame should be filter conditions which can't yet be computed because they
reference unbound variables. Whenever the frame is extended, the extension fails
if there is a promise which can be computed and that promise does not pass. If
it does pass, the promise is removed from the list.

What if a variable is never bound? Consider the following query from 4.56:

(and (supervisor ?person ?super)
     (job ?super . ?any0)
     (not (job ?super (computer . ?any1))))

In the original procedural query system the user intended the variable to be
unbound. In the new system what does the user mean? Two choices are (1) they
mean the unbound variable in the not to bind to anything or (2) they mean the
unbound variable to cause the clause to fail. Arbitrarily I chose the latter. It
makes not and lisp-value consistant although more in-depth design analysis would
be needed for a finished system.

NOTE: This modification is similar to 4.76, in that it takes the query system a
step towards mathematical logic. These prompts should have hinted that these
exercises involve rudimentary tweaks that take the system towards mathematical
logic, but we're not following that idea to completion. These changes may
eliminate positive aspects of the procedural query system. Some design choices
they may invalidate or change the meaning of syntax.

In a procedural query system a possible improvement is that filter operations
against unbound variables throw an error. Other useful improvements to the
procedural query system might be to differentiate between "bind to anything" and
"only accept things that are already bound" especially for filter clauses.

In a logic based query system, a better architecture would probably be to
analyze the query ahead of time, perhaps rearranging the order in which clauses
are evaluated. At this time, an error could be thrown if a variable in a filter
clause is never bound and the user hasn't specifically marked this variable as a
binding to anything.

|#

#| Evaluator (modified from 4.55) |#

;;; (make-frame bindings '())

(define (make-is-ready-func query)
  (lambda (f)
    (let ([ret #t])
      (instantiate query f (lambda (v f) (set! ret #f)))
      ret)))

(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let* ([query (negated-query operands)]
             [is-ready (make-is-ready-func query)]
             [passes (lambda (f)
                       (stream-null? (qeval query (singleton-stream f))))]
             [extended-frame (extend-promise is-ready passes frame)])
        (if (eq? extended-frame 'failed)
            the-empty-stream
            (singleton-stream extended-frame))))
    frame-stream))

(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let* ([is-ready (make-is-ready-func call)]
             [passes (lambda (f)
                       (execute (instantiate call f (lambda (v f) (error "lisp-value" "impossible")))))]
             [extended-frame (extend-promise is-ready passes frame)])
        (if (eq? extended-frame 'failed)
            the-empty-stream
            (singleton-stream extended-frame))))
    frame-stream))

#| Frames and Bindings (modified from 4.55) -- frame is (cons bindings promises) |#

;;; promises
(define (make-promise is-ready-func passes-func)
  (cons is-ready-func passes-func))
(define (get-is-ready-func p)
  (car p))
(define (get-passes-func p)
  (cdr p))

;;; frames
(define (make-frame bindings promises)
  (cons bindings promises))
(define (get-bindings frame)
  (if (null? frame)
      frame
      (car frame)))
(define (get-promises frame)
  (if (null? frame)
      frame
      (cdr frame)))
(define (binding-in-frame variable frame)
  (assoc variable (get-bindings frame)))
(define (extend variable value frame)
  (apply-promises (make-frame (cons (make-binding variable value) (get-bindings frame))
                              (get-promises frame))))
(define (extend-promise is-ready-func passes-func frame)
  (apply-promises (make-frame (get-bindings frame)
                              (cons (make-promise is-ready-func passes-func) (get-promises frame)))))
(define (apply-promises frame)
  (define (iter unbound-promises bindings promises)
    (if (null? promises)
        (make-frame bindings unbound-promises)
        (let* ([f (make-frame bindings '())]
               [head (car promises)]
               [is-ready (get-is-ready-func head)]
               [passes (get-passes-func head)]
               [rest (cdr promises)])
          (if (is-ready f)
              (begin
                ;(break)
                (if (passes f)
                    (begin #|(display "\npass")|# (iter unbound-promises bindings rest))
                    (begin #|(display "\nfail")|# 'failed)))
              (begin 
                (iter (cons head unbound-promises) bindings rest))))))
  (iter '() (get-bindings frame) (get-promises frame)))
    
#| Updatedated custom infrastructure |#
(define (top-level-qeval q)
  (stream-flatmap
    (lambda (frame)
        (if (null? (get-promises frame))
            (singleton-stream frame)
            the-empty-stream))
    (qeval q (singleton-stream '()))))

#| Tests -- two regression tests invloving unbound nots now fail |#

;;; flipped order from 4.55
(define-test (do-query test-db 
  '(and (not (job ?x (computer programmer)))
        (job ?x . ?details)))
  '((and (not (job (Bitdiddle Ben) (computer programmer)))
         (job (Bitdiddle Ben) (computer wizard)))
    (and (not (job (Tweakit Lem E) (computer programmer)))
         (job (Tweakit Lem E) (computer technician)))
    (and (not (job (Reasoner Louis) (computer programmer)))
         (job (Reasoner Louis) (computer programmer trainee)))
    (and (not (job (Warbucks Oliver) (computer programmer)))
         (job (Warbucks Oliver) (administration big wheel)))
    (and (not (job (Scrooge Eben) (computer programmer)))
         (job (Scrooge Eben) (accounting chief accountant)))
    (and (not (job (Cratchet Robert) (computer programmer)))
         (job (Cratchet Robert) (accounting scrivener)))
    (and (not (job (Aull DeWitt) (computer programmer)))
         (job (Aull DeWitt) (administration secretary)))))

;;; flipped order from 4.55
(define-test (do-query test-db '(and (lisp-value > ?amount 30000)
                                     (salary ?person ?amount)))
  '((and (lisp-value > 60000 30000)
         (salary (Bitdiddle Ben) 60000))
    (and (lisp-value > 40000 30000)
         (salary (Hacker Alyssa P) 40000))
    (and (lisp-value > 35000 30000)
         (salary (Fect Cy D) 35000))
    (and (lisp-value > 150000 30000)
         (salary (Warbucks Oliver) 150000))
    (and (lisp-value > 75000 30000)
         (salary (Scrooge Eben) 75000))))
