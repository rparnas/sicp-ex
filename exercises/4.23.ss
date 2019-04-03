#|

Exercise 4.23: Alyssa P. Hacker doesn't understand why
"analyze-sequence" needs to be so complicated. All the other
analysis procedures are straightforward transformations of
the corresponding evaluation procedures (or "eval" clauses)
in Section 4.1.1. She expected "analyze-sequence" to look
like this:

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
           ((car procs) env)
           (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (lambda (env)
      (execute-sequence procs env))))

Eva Lu Ator explains to Alyssa that the version in the text
does more of the work of evaluating a sequence at analysis
time. Alyssa's sequence-execution procedure, rather than
having the calls to the individual execution procedures
built in, loops through the procedures in order to call
them: In effect, although the individual expressions in the
sequence have been analyzed, the sequence itself has not
been.

Compare the two versions of "analyze-sequence". For example,
consider the common case (typical of procedure bodies) where
the sequence has just one expression. What work will the
execution procedure produced by Alyssa's program do? What
about the execution procedure produced by the program in the
text above? How do the two versions compare for a sequence
with two expressions?

|#

(load-ex "4.22")

#| Code from Book |#

(define analyze-sequence-422 analyze-sequence) ;;; original from 4.22 

;;; new version from 4.23
(define (analyze-sequence-423 exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
           ((car procs) env)
           (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "analyze" "empty sequence"))
    (lambda (env)
      (execute-sequence procs env))))

#| Answer

The original version returns procedures like:
  p0

  (lambda (env) 
    (p0 env) 
    (p1 env))

  (lambda (env)
    ((lambda (env) 
      (p0 env) 
        (p1 env)) env)
  (p2 env))

The new version returns procedures like:
  (lambda (env) (execute-sequence '(p0) env))
  (lambda (env) (execute-sequence '(p0 p1) env))
  (lambda (env) (execute-sequence '(p0 p2) env))

The original has a nice optimization for a single item. The results of the
original seem unweildly to read as additional item are added. It also seems like
going to be evaluating a lot of procedures which involve setting up
environments, etc.

This suggests the new version might be faster especially if the underlying
scheme has tail-call recursion to the benefit of the new version. But testing
performed below doesn't show performance as being an issue with either approach.

I'd pick the new version for readability. I'd make a minor modification: a
sequence of one item should just return the resultant procedure.

|#

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
           ((car procs) env)
           (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (cond [(null? procs)
           (error "analyze-sequence" "empty sequence")]
          [(null? (cdr procs))
           (car procs)]
          [else 
           (lambda (env)
            (execute-sequence procs env))])))

#| Tests

> (define env 0)
> (define (analyze exp) (lambda (env) (random 10)))
> (define (make-exps n) (iota n))
> (define (time-thunk thunk)
    (define start (cpu-time))
    (define result (thunk))
    (define stop (cpu-time))
    (define elapsed (- stop start))
  elapsed)

> (define a422 (analyze-sequence-422 (iota 100000)))
> (list (time-thunk (lambda () (a422 env)))
        (time-thunk (lambda () (a422 env)))
        (time-thunk (lambda () (a422 env))))
(0 0 0)

> (define a423 (analyze-sequence-423 (iota 100000)))
> (list (time-thunk (lambda () (a423 env)))
        (time-thunk (lambda () (a423 env)))
        (time-thunk (lambda () (a423 env))))
(0 0 0)

|#