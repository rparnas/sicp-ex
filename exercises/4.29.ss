#|

Exercise 4.29: Exhibit a program that you would expect to
run much more slowly without memoization than with
memoization. Also, consider the following interaction, where
the "id" procedure is defined as in Exercise 4.27 and
"count" starts at 0:

(define (square x) (* x x))
 ;;; L-Eval input: 
(square (id 10))
 ;;; L-Eval value: 
<response>
 ;;; L-Eval input: 
count
 ;;; L-Eval value: 
<response>

Give the responses both when the evaluator memoizes and when it does not.

|#


#| Answer

Any procedure where an argument is referenced more than once will experience a
slowdown. This slowdown will be worse in recursive procedures where the
arguments build up complexity as they get deeper down. For example Fibonacci is
about 5.8 times slower for (fib 30).

In the example, the output will be (100 1) with memoization and (100 2) without.
This makes sense because the square procedure references its single argument
twice.

|#

#| Tests

> (reset-ex "4.27")
> (define (time-thunk thunk)
   (define start (cpu-time))
   (define result (thunk))
   (define stop (cpu-time))
   (define elapsed (- stop start))
   elapsed)

;;;; memoized (fib 30) -- averages to 1359ms
> (define exp '(begin 
                 (define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
                 (fib 30)))
> (define env (setup-environment))
> (time-thunk (lambda () (eval exp env)))

;;; non-memoized (fib 30) -- averages to 7943ms
> (define (force-it obj)
    (if (thunk? obj)
        (actual-value (thunk-exp obj) (thunk-env obj))
        obj))
> (define exp '(begin 
                 (define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
                 (fib 30)))
> (define env (setup-environment))
> (time-thunk (lambda () (eval exp env)))

;;;; memoized -- square invokes (id 10) once
> (define e (setup-environment))
> (eval '(define count 0) e)
> (eval '(define (id x) (set! count (+ count 1)) x) e)
> (eval '(define (square x) (* x x)) e)
> (force-it (eval '(square (id 10)) e))
100
> (force-it (eval 'count e))
1

;;;; not memoized -- square invokes (id 10) twice
> (define (force-it obj)
    (if (thunk? obj)
        (actual-value (thunk-exp obj) (thunk-env obj))
        obj))
> (define e (setup-environment))
> (eval '(define count 0) e)
> (eval '(define (id x) (set! count (+ count 1)) x) e)
> (eval '(define (square x) (* x x)) e)
> (force-it (eval '(square (id 10)) e))
100
> (force-it (eval 'count e))
2

|#