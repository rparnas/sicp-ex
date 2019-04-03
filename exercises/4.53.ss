#|

Exercise 4.53: With "permanent-set!" as described in
Exercise 4.51 and "if-fail" as in Exercise 4.52, what will
be the result of evaluating

(let ((pairs '()))
  (if-fail
   (let ((p (prime-sum-pair '(1 3 5 8)
                            '(20 35 110))))
     (permanent-set! pairs (cons p pairs))
     (amb))
   pairs))

|#

(load-ex "4.52")
(no-regression)

#| Code from book |#
(define setup-environment-452 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-452)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))
    ;;; 1.21
    (add '(define (square x)
            (* x x)))
    (add '(define (smallest-divisor n)
            (find-divisor n 2)))
    (add '(define (find-divisor n test-divisor)
      (cond [(> (square test-divisor) n) n]
      [(divides? test-divisor n) test-divisor]
      [else (find-divisor n (+ test-divisor 1))])))
    (add '(define (divides? a b)
      (= (remainder b a) 0)))
    (add '(define (prime? n)
      (and (> n 1) (= n (smallest-divisor n)))))
    ;;; code from book
    (add '(define (prime-sum-pair list1 list2)
            (let ((a (one-from-set list1))
                  (b (one-from-set list2)))
              (require (prime? (+ a b)))
              (list a b))))
    ;;; return
    env))


#| Answer 

It returns (8 35) (3 110) (3 20).

Every time it escapes out of prime-sum-pair with a valid
pair, it adds the valid pair to pairs. It then hits (amb)
which causes a failure, it goes back to prime-sum-pair and
then add it to pairs again. The key is permanent-set! is
called multiple times and none of the calls is undone.

|#

#| Tests |#
(define-test (eval-all
  '(let ((pairs '()))
    (if-fail
     (let ((p (prime-sum-pair '(1 3 5 8)
                              '(20 35 110))))
       (permanent-set! pairs (cons p pairs))
       (amb))
     pairs)))
  '(((8 35) (3 110) (3 20))))