#|

Exercise 5.2: Use the register-machine language to describe
the iterative factorial machine of Exercise 5.1.

|#

(load-ex "5.1")

#| Answer |#
(define (factorial-iterative-machine)
  (make-machine 
    '(product counter n) 
    (list (list '> >) 
          (list '* *) 
          (list '+ +))
    '((assign product (const 1))
      (assign counter (const 1))
      start
      (test (op >) (reg counter) (reg n))
      (branch (label done))
      (assign product (op *) (reg product) (reg counter))
      (assign counter (op +) (reg counter) (const 1))
      (goto (label start))
      done)))

#| Tests |#
(define-test (let ([m (factorial-iterative-machine)]) (begin
  (set-register-contents! m 'n 3)
  (start m)
  (get-register-contents m 'product)))
6)
