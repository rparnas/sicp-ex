#|

Exercise 4.68: Define rules to implement the "reverse"
operation of Exercise 2.18, which returns a list containing
the same elements as a given list in reverse order. (Hint:
Use "append".) Can your rules answer both "(reverse
(1 2 3) ?x)" and "(reverse ?x (1 2 3))" ?

|#

(load-ex "4.67")

#| Answer

If the append line is first, it only works backwards and if the append line is
second it only works forwards.

|#

(define db468 '(
  ; Code from book
  (rule (append-to-form () ?y ?y))
  (rule (append-to-form (?u . ?v) ?y (?u . ?z))
        (append-to-form ?v ?y ?z))
  ;;; answer
  (rule (reverse () ()))
  (rule (reverse (?head . ?rest) ?rev)
        (and (append-to-form ?rev-rest (?head) ?rev)
             (reverse ?rest ?rev-rest)))))

#| Tests |#
(define-test (do-query db468 
  '(reverse ?x ()))
  '((reverse () ())))
(define-test (do-query db468 
  '(reverse () ?x))
  '((reverse () ())))
(define-test (do-query db468 
  '(reverse ?x (0)))
  '((reverse (0) (0))))
(define-test (do-query db468 ; relies on 4.67 loop detection
  '(reverse (0) ?x))
  '((reverse (0) (0))))
(define-test (do-query db468 
  '(reverse ?x (3 2 1)))
  '((reverse (1 2 3) (3 2 1))))
(define-test (do-query db468 ; incorrect answer, relies on 4.67 loop detection.
  '(reverse (1 2 3) ?x))
  '())
