#|

Exercise 4.24: Design and carry out some experiments to
compare the speed of the original metacircular evaluator
with the version in this section. Use your results to
estimate the fraction of time that is spent in analysis
versus execution for various procedures.

|#

#| Answer 

Running all regression tests takes a negligable amount of time.Using the non-
memoized fib procedure it takes (fib 40) an average of 369ms in my underlying
scheme. For (fib 30):
  * unerlying scheme interpreter: ~0ms
  * eval-only-interpreter: 1318ms
  * analyze/invoke interpreter: (~0ms analyze) + (448ms invoke) = 448ms

Perhaps eval-only wastes 66% of its time re-analyzing the fib procedure on each
recursive call.

If I were making a scheme interpreter I'd start with the analyze/invoke
architecture but have the analysis results be an intermediate human-readable
scheme data structure rather than a ready-to-go scheme procedure. It seems like
a waste to ask the developer to keep the mental model of "the results of
analyzing a particular expression" in their heads especially when it comes to
debugging or providing better analysis results.

For example, I can see patterns manually or write procedures to analyze scheme
data structures but scheme is limited in my ability to write code which analyzes
procedures.  What if I want to analyze the results of analysis? Or split the
analysis into multiple steps?

|#

#| Tests 

> (define (time-thunk thunk)
   (define start (cpu-time))
   (define result (thunk))
   (define stop (cpu-time))
   (define elapsed (- stop start))
   elapsed)

;;;; eval-only interpreter
> (reset-ex "4.21")
> (define test-exps (map test-expression (fold-right append '() (map test-set-tests all-tests))))
> (define exps (map cadr test-exps))
> (define envs (map (lambda (exp) (setup-environment)) exps))
> (time-thunk (lambda () (map (lambda (exp env) (eval exp env)) exps envs)))
0

> (reset-ex "4.21")
> (define exp '(begin 
                 (define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
                 (fib 30)))
> (define env (setup-environment))
> (time-thunk (lambda () (eval exp env)))

;;;; analyze invoke interpreter
> (reset-ex "4.22")
> (define test-exps (map test-expression (fold-right append '() (map test-set-tests all-tests))))
> (define exps (map cadr test-exps))
> (define envs (map (lambda (exp) (setup-environment)) exps))
> (time-thunk (lambda () (map (lambda (exp env) (eval exp env)) exps envs)))
0

> (reset-ex "4.22")
> (define exp '(begin 
                 (define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
                 (fib 30)))
> (define env (setup-environment))
> (define ana 'undefined)
> (time-thunk (lambda () (set! ana (analyze exp))))
> (time-thunk (lambda () (ana env)))

|#