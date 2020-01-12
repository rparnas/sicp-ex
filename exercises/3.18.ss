#|

Exercise 3.18: Write a procedure that examines a list and
determines whether it contains a cycle, that is, whether a
program that tried to find the end of the list by taking
successive "cdr"s would go into an infinite loop. Exercise
3.13 constructed such lists.

|#

#| Answer 

Answer below is incorrect -- needs to be updated.

|#

(define (is-cycle? ls)
  (define (iter x)
    (cond [(not (pair? x))
           #f]
          [(eq? (car ls) (car x))
           #t]
          [else (iter (cdr x))]))
  (if (not (pair? ls))
      #f
      (iter (cdr ls))))

#| Test |#
(define-test (is-cycle? 1) #f)

(define-test (is-cycle? '(1 2 3)) #f)

(define-test (begin
              (define x (cons 1 (cons 2 (cons 3 '()))))
              (append! x x)
              (is-cycle? x))
             #t)