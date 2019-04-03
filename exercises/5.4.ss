#|

Exercise 5.4: Specify register machines that implement each
of the following procedures. For each machine, write a
controller instruction sequence and draw a diagram showing
the data paths.

a. Recursive exponentiation:

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

b. Iterative exponentiation:

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))

|#

(load-ex "5.3")

#| Answer -- skipped drawings |#

(define (expt-recur-m)
  (make-machine
    '(b n continue ret)
    (list (list '= =)
          (list '- -)
          (list '* *))
    '((assign continue (label done))
      expt
      (test (op =) (reg n) (const 0))
      (branch (label expt-base-case))
      ;;; prepare for recursive call
      (save continue)
      (assign continue (label after-recur))
      (assign n (op -) (reg n) (const 1))
      (goto (label expt))
      after-recur
      (restore continue)
      (assign ret (op *) (reg b) (reg ret))
      (goto (reg continue))
      expt-base-case
      (assign ret (const 1))
      (goto (reg continue))
      done)))
  (let ([m (expt-recur-m)]) (begin
    (set-register-contents! m 'b 3)
    (set-register-contents! m 'n 4)
    (start m)
    (get-register-contents m 'ret)))

(define (expt-iter-m)
  (make-machine
    '(b n counter product)
    (list (list '= =)
          (list '- -)
          (list '* *))
    '((assign counter (reg n))
      (assign product (const 1))
      expt-iter
      (test (op =) (reg counter) (const 0))
      (branch (label done))
      (assign counter (op -) (reg counter) (const 1))
      (assign product (op *) (reg b) (reg product))
      (goto (label expt-iter))
      done)))

#| Tests |#
(define-test (let ([m (expt-recur-m)]) (begin
  (set-register-contents! m 'b 3)
  (set-register-contents! m 'n 4)
  (start m)
  (get-register-contents m 'ret)))
  81)

(define-test (let ([m (expt-iter-m)]) (begin
  (set-register-contents! m 'b 3)
  (set-register-contents! m 'n 4)
  (start m)
  (get-register-contents m 'product)))
  81)