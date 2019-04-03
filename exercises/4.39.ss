#|

Exercise 4.39: Does the order of the restrictions in the
multiple-dwelling procedure affect the answer? Does it
affect the time to find an answer? If you think it matters,
demonstrate a faster program obtained from the given one by
reordering the restrictions. If you think it does not
matter, argue your case.

|#

(load-ex "4.38")

#| Answer 

The order of restrictions does not affect the answer. Neighboring requires are
basically an "and" operation.

There is no amb value followed by a require. This means no requirement restricts
the possibility sets that will be examined.

Runtime is affected by the complexity of individual require cases. An optimal
solution requires analyzing the expense of a requirement and the probability a
requirement fails (thus short-circuiting the need to test additional
requirements).

A naive optimization is to move distinct to the end because it performs
Theta(n^2) comparisons with respect to the size of the list. This only helps if
the other requirements short-circuit often enough.

Based on the testing below, this optimization reduces the number of top level
calls to distinct from 3125 to 120.

However any version of the algorithm seems to take a trivial amount of time
suggesting the amb system predominates. So perhaps we should focus on reducing
the amb search space.

|#

#| Tests 

> (define (time-thunk thunk)
    (define start (cpu-time))
    (define result (thunk))
    (define stop (cpu-time))
    (define elapsed (- stop start))
  elapsed)
> (define (time-thunk-1000 thunk)
    (let ([elapsed (map (lambda (i) (time-thunk thunk)) (iota 1000))])
      (/ (fold-right + 0.0 elapsed) (length elapsed))))

;;; test 1 from 4.38 -- 4.859ms average
> (time-thunk-1000 (lambda () (eval-all
    '((lambda ()
        (let ([baker (amb 1 2 3 4 5)]
              [cooper (amb 1 2 3 4 5)]
              [fletcher (amb 1 2 3 4 5)]
              [miller (amb 1 2 3 4 5)]
              [smith (amb 1 2 3 4 5)])
          (require (distinct? (list baker cooper fletcher miller smith)))
          (require (not (= baker 5)))
          (require (not (= cooper 1)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
          (require (> miller cooper))
          (require (not (= (abs (- smith fletcher)) 1)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith))))))))
4.859

;;; distinct test moved to end -- 4.796ms average
> (time-thunk-1000 (lambda () (eval-all
    '((lambda ()
        (let ([baker (amb 1 2 3 4 5)]
              [cooper (amb 1 2 3 4 5)]
              [fletcher (amb 1 2 3 4 5)]
              [miller (amb 1 2 3 4 5)]
              [smith (amb 1 2 3 4 5)])
          (require (not (= baker 5)))
          (require (not (= cooper 1)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
          (require (> miller cooper))
          (require (not (= (abs (- smith fletcher)) 1)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith))))))))
4.796

;;;; total possibility sets
> (length (eval-all 
   '((lambda ()
       (let ([baker (amb 1 2 3 4 5)]
             [cooper (amb 1 2 3 4 5)]
             [fletcher (amb 1 2 3 4 5)]
             [miller (amb 1 2 3 4 5)]
             [smith (amb 1 2 3 4 5)]) 0)))))
3125

;;; how many possibilities sets get past every check except distict
(length (eval-all '(let ([baker (amb 1 2 3 4 5)]
                [cooper (amb 1 2 3 4 5)]
                [fletcher (amb 1 2 3 4 5)]
                [miller (amb 1 2 3 4 5)]
                [smith (amb 1 2 3 4 5)])
            (require (not (= baker 5)))
            (require (not (= cooper 1)))
            (require (not (= fletcher 5)))
            (require (not (= fletcher 1)))
            (require (> miller cooper))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require (not (= (abs (- fletcher cooper)) 1)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith)))))
120

|#