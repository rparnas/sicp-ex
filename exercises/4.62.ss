#|

Exercise 4.62: Define rules to implement the "last-pair"
operation of Exercise 2.17, which returns a list containing
the last element of a nonempty list. Check your rules on
queries such as "(last-pair (3) ?x)", "(last-pair (1 2 3)
?x)" and "(last-pair (2 ?x) (3))". Do your rules work
correctly on queries such as "(last-pair ?x (3))" ?

|#

(load-ex "4.61")

#| Answers 

;;; 2.17
(define (last-pair ls)
  (if (null? (cdr ls))
      ls
      (last-pair (cdr ls))))

|#

(define db462 '(
  (rule (last-pair (?last-pair) (?last-pair)))
  (rule (last-pair (?head . ?rest) (?last-pair))
        (last-pair ?rest (?last-pair)))))

#| Tests |#
(define-test (do-query db462
  '(last-pair (3) ?x))
  '((last-pair (3) (3))))

(define-test (do-query db462
  '(last-pair (1 2 3) ?x))
  '((last-pair (1 2 3) (3))))

(define-test (do-query db462
  '(last-pair (2 ?x) (3)))
  '((last-pair (2 3) (3))))