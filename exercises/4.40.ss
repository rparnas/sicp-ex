#|

Exercise 4.40: In the multiple dwelling problem, how many
sets of assignments are there of people to floors, both
before and after the requirement that floor assignments be
distinct? It is very inefficient to generate all possible
assignments of people to floors and then leave it to
backtracking to eliminate them. For example, most of the
restrictions depend on only one or two of the person-floor
variables, and can thus be imposed before floors have been
selected for all the people. Write and demonstrate a much
more efficient nondeterministic procedure that solves this
problem based upon generating only those possibilities that
are not already ruled out by previous restrictions. (Hint:
This will require a nest of "let" expressions.)

|#

(load-ex "4.39")

#| Answer 

5^5 = 3125 possibility sets are explored before checking for distinctness. 

A distinct check involves 10 comparisons (for a total of 31250 comparisons).

5! = 120 possibilities sets pass the distinct check.

To improve efficiency I tried integrating the distinct check into the number
choices. This seems to reduce runtime from 4.859ms to less than 1ms on average
versus 4.39 (although it is questionable how reliably I can measure the
performance of such small operations).

You could interleave requirements and ambiguous values for more gains but I
think this is a bad approach because it requires a lot of analysis to determine
the optimal order. It also isn't very robust as optimal order could be thrown
off by requirement changes. In general the interleave strategy could be a case
of encoding partial solutions within that which is supposed to represent the
problem statement.

I think it would be good to introduce a "permutations" procedure which
ambiguously returns all permutations of a list. This reduces the ambigous value
space explored while still keeping the code well mapped to the problem
statement.

|#

#| Answer |#
(define setup-environment-439 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-439)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))

    ;;; for try 1
    (add '(define (any-except items not-items)
            (cond [(null? items) (amb)]
                  [(member (car items) not-items)
                   (any-except (cdr items) not-items)]
                  [else (amb (car items) (any-except (cdr items) not-items))])))

    ;;; for final optimization -- (see 2.41)
    (add '(define (one-from-set set)
            (if (null? set)
                (amb)
                (amb (car set) (one-from-set (cdr set))))))
    (add '(define (remove-from-set set x)
            (define (iter result set)
              (cond [(null? set) result]
                    [(equal? (car set) x) (iter result (cdr set))]
                    [else (iter (append result (list (car set))) (cdr set))]))
            (iter '() set)))
    (add '(define (permutations set)
            (if (null? set)
                (list)
                (let* ([first (one-from-set set)]
                       [rest (permutations (remove-from-set set first))])
                  (append (list first) rest)))))
    ;;; return
    env))
#| Tests |#

;;; before requirement of distinctness
(define-test (length (eval-all 
  '((lambda ()
      (let ([baker (amb 1 2 3 4 5)]
            [cooper (amb 1 2 3 4 5)]
            [fletcher (amb 1 2 3 4 5)]
            [miller (amb 1 2 3 4 5)]
            [smith (amb 1 2 3 4 5)]) 0)))))
  3125)

;;; after requirement of distinctness
(define-test (length (eval-all 
  '((lambda ()
      (let ([baker (amb 1 2 3 4 5)]
            [cooper (amb 1 2 3 4 5)]
            [fletcher (amb 1 2 3 4 5)]
            [miller (amb 1 2 3 4 5)]
            [smith (amb 1 2 3 4 5)])
      (require (distinct? (list baker cooper fletcher miller smith))))))))
  120)

;;; infrastructure test
(define-test (eval-all 
  '(any-except '(1 2 3) '(1))) 
  '(2 3))

;;; my improvement 1 -- amb system only proceduces 120 possibilities
(define-test (length (eval-all 
  '((lambda ()
     (let ([baker (any-except '(1 2 3 4 5) (list))])
      (let ([cooper (any-except '(1 2 3 4 5) (list baker))])
        (let ([fletcher (any-except '(1 2 3 4 5) (list baker cooper))])
          (let ([miller (any-except '(1 2 3 4 5) (list baker cooper fletcher))])
            (let ([smith (any-except '(1 2 3 4 5) (list baker cooper fletcher miller))])
              0)))))))))
  120)

#| my improvement 1 -- timed

> (define (time-thunk thunk)
    (define start (cpu-time))
    (define result (thunk))
    (define stop (cpu-time))
    (define elapsed (- stop start))
  elapsed)
> (define (time-thunk-1000 thunk)
    (let ([elapsed (map (lambda (i) (time-thunk thunk)) (iota 1000))])
      (/ (fold-right + 0.0 elapsed) (length elapsed))))

> (time-thunk-1000 (lambda () (eval-all
  '((lambda ()
     (let ([baker (any-except '(1 2 3 4 5) (list))])
      (let ([cooper (any-except '(1 2 3 4 5) (list baker))])
        (let ([fletcher (any-except '(1 2 3 4 5) (list baker cooper))])
          (let ([miller (any-except '(1 2 3 4 5) (list baker cooper fletcher))])
            (let ([smith (any-except '(1 2 3 4 5) (list baker cooper fletcher miller))])
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
                (list 'smith smith))))))))))))
0.859

|#


;;; my improvement 2 -- permutation system
(define-test (length (eval-all '(permutations '(1 2 3 4 5))))
             120)

(define-test (eval-all 
  '((lambda ()
     (let* ([floors (permutations '(1 2 3 4 5))]
            [baker (car floors)]
            [cooper (car (cdr floors))]
            [fletcher (car (cdr (cdr floors)))]
            [miller (car (cdr (cdr (cdr floors))))]
            [smith (car (cdr (cdr (cdr (cdr floors)))))])
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
             (list 'smith smith))))))
  '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))))

#| my improvement 2 -- permutation system

> (time-thunk-1000 (lambda () (eval-all 
  '((lambda ()
     (let* ([floors (permutations '(1 2 3 4 5))]
            [baker (car floors)]
            [cooper (car (cdr floors))]
            [fletcher (car (cdr (cdr floors)))]
            [miller (car (cdr (cdr (cdr floors))))]
            [smith (car (cdr (cdr (cdr (cdr floors)))))])
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
1.562ms

|#