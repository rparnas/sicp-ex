#|

Exercise 5.21: Implement register machines for the following
procedures. Assume that the list-structure memory operations
are available as machine primitives.

a. Recursive "count-leaves":

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

b. Recursive "count-leaves" with explicit counter:

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else
           (count-iter (cdr tree)
                       (count-iter (car tree)
                                   n)))))
  (count-iter tree 0))

|#

(load-ex "5.19")

#| Answer |#
(define (count-leaves-iter-machine) (make-machine
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'not not)
        (list 'null? null?)
        (list 'pair? pair?)
        (list '+ +))
  '((assign continue (label done))

    count-leaves
    (test (op null?) (reg tree))
    (branch (label zero-case))
    (assign temp (op pair?) (reg tree))
    (test (op not) (reg temp))
    (branch (label one-case))

    car-recur
    (save continue)
    (save tree)
    (assign continue (label after-car-recur))
    (assign tree (op car) (reg tree))
    (goto (label count-leaves))

    after-car-recur
    (restore tree)
    (restore continue)

    cdr-recur
    (save continue)
    (save tree)
    (save ret)
    (assign continue (label after-cdr-recur))
    (assign tree (op cdr) (reg tree))
    (goto (label count-leaves))

    after-cdr-recur
    (assign temp (reg ret)) ; temp -- (count-leaves (cdr tree))
    (restore ret)           ; ret -- (count-leaves (car tree))
    (restore tree)
    (restore continue)
    (assign ret (op +) (reg temp) (reg ret))
    (goto (reg continue))

    zero-case
    (assign ret (const 0))
    (goto (reg continue))

    one-case
    (assign ret (const 1))
    (goto (reg continue))

    done)))

(define (count-leaves-recur-machine) (make-machine
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'not not)
        (list 'null? null?)
        (list 'pair? pair?)
        (list '+ +))
  '((assign continue (label done))
    (assign n (const 0))

    count-iter
    (test (op null?) (reg tree))
    (branch (label n-case))
    (assign temp (op pair?) (reg tree))
    (test (op not) (reg temp))
    (branch (label n-plus-one-case))

    count-iter-recur
    (save continue)
    (save tree)
    (assign continue (label after-count-iter-recur))
    (assign tree (op car) (reg tree))
    (goto (label count-iter))

    after-count-iter-recur
    (restore tree)
    (restore continue)
    (assign tree (op cdr) (reg tree))
    (assign n (reg ret))
    (goto (label count-iter))

    n-case
    (assign ret (reg n))
    (goto (reg continue))

    n-plus-one-case
    (assign ret (op +) (reg n) (const 1))
    (goto (reg continue))

    done)))

#| Tests -- example from 2.35 |#
(define-test (let ([m (count-leaves-iter-machine)])
  (set-register-contents! m 'tree '())
  (start m)
  (get-register-contents m 'ret))
  0)

(define-test (let ([m (count-leaves-iter-machine)])
  (set-register-contents! m 'tree (list (list 1 2) (list 1 2 3) 1))
  (start m)
  (get-register-contents m 'ret))
  6)

(define-test (let ([m (count-leaves-recur-machine)])
  (set-register-contents! m 'tree '())
  (start m)
  (get-register-contents m 'ret))
  0)

(define-test (let ([m (count-leaves-recur-machine)])
  (set-register-contents! m 'tree (list (list 1 2) (list 1 2 3) 1))
  (start m)
  (get-register-contents m 'ret))
  6)