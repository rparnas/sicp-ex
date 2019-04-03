#|

Exercise 5.22: Exercise 3.12 of Section 3.3.1 presented an
"append" procedure that appends two lists to form a new list
and an "append!" procedure that splices two lists together.
Design a register machine to implement each of these
procedures. Assume that the list-structure memory operations
are available as primitive operations.

|#

(load-ex "5.21")

#| Code from book -- from 3.12 

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

|#

#| Answer |#
(define (append-machine) (make-machine
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?))
  '((assign continue (label done))

    append
    (test (op null?) (reg x))
    (branch (label base-case))

    before-recur
    (save continue)
    (save x)
    (assign continue (label after-recur))
    (assign x (op cdr) (reg x))
    (goto (label append))

    after-recur
    (restore x)
    (restore continue)
    (assign x (op car) (reg x))
    (assign ret (op cons) (reg x) (reg ret))
    (goto (reg continue))

    base-case
    (assign ret (reg y))
    (goto (reg continue))

    done)))

(define (append!-machine) (make-machine
  (list (list 'cdr cdr)
        (list 'null? null?)
        (list 'set-cdr! set-cdr!))
  '((save x)
    last-pair
    (assign temp (op cdr) (reg x))
    (test (op null?) (reg temp))
    (branch (label after-last-pair)) ; x is the last-pair of the original x.
    (assign x (op cdr) (reg x))
    (goto (label last-pair))
    after-last-pair
    (perform (op set-cdr!) (reg x) (reg y))
    (restore x))))

#| Tests |#
(define-test (let ([m (append-machine)]) (begin
  (set-register-contents! m 'x '(1 2))
  (set-register-contents! m 'y '(3 4))
  (start m)
  (get-register-contents m 'ret)))
  '(1 2 3 4))

;;; Must make a new ls0 and ls1 every time the test is run b/c ls0 is mutated.
(define-test (let ([ls0 (list 1 2)]
                   [ls1 (list 3 4)]
                   [m (append!-machine)]) (begin
  (set-register-contents! m 'x ls0)
  (set-register-contents! m 'y ls1)
  (start m)
  (get-register-contents m 'x)))
  '(1 2 3 4))