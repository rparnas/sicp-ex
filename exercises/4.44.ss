#|

Exercise 4.44: Exercise 2.42 described the "eight-queens
puzzle" of placing queens on a chessboard so that no two
attack each other. Write a nondeterministic program to solve
this puzzle.

|#

(load-ex "4.43")

#| Answer |#
(define setup-environment-443 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-443)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))

    ;;; returns 1 solutions to n-queens problem
    (add '(define (queens n)
            (define (make-queens n)
              (define (my-iota n)
                (define (iter m)
                  (if (> m n)
                      (list)
                      (cons m (iter (+ m 1)))))
                (iter 1))
              (define (iter row-index available-cols)
                (if (> row-index n)
                    (list)
                    (let ([col-index (one-from-set available-cols)])
                      (append (list (cons row-index col-index))
                              (iter (+ row-index 1)
                                    (remove-from-set available-cols col-index))))))
              (iter 1 (my-iota n)))
            ;;; returns true if the given position is safe
            (define (safe? i positions)
              (define (pos-row pos) (car pos))
              (define (pos-col pos) (cdr pos))
              (define (none? p seq) (null? (filter p seq)))
              (define (filter proc ls)
                (define (iter result ls)
                  (cond [(null? ls) result]
                        [(proc (car ls)) (iter (append result (list (car ls))) (cdr ls))]
                        [else (iter result (cdr ls))]))
                (iter '() ls))
              (define (ith-element ls i)
                (if (= i 0)
                    (car ls)
                    (ith-element (cdr ls) (- i 1))))
              (let* ([ith (ith-element positions i)]
                     [col (pos-col ith)]
                     [row (pos-row ith)]
                     [others (remove-from-set positions ith)])
                (and (none? (lambda (o) (= (pos-row o) row)) others)
                     (none? (lambda (o) (= (pos-col o) col)) others)
                     (none? (lambda (o) (= (abs (- col (pos-col o))) (abs (- row (pos-row o))))) others))))
            (let* ([queens (make-queens n)])
               (define (all-safe i queens)
                 (if (= i (length queens))
                     queens
                     (begin
                       (require (safe? i queens))
                       (all-safe (+ i 1) queens))))
               (all-safe 0 queens))))
    ;;; return
    env))

#| Tests -- same as 2.42 |#
(define-test (eval-all '(queens 1))
             '(((1 . 1))))

(define-test (eval-all '(queens 2))
             '())

(define-test (eval-all '(queens 3))
             '())

(define-test (eval-all '(queens 4))
             '(((1 . 2) (2 . 4) (3 . 1) (4 . 3))
               ((1 . 3) (2 . 1) (3 . 4) (4 . 2))))

(define-test (length (eval-all '(queens 5))) 10)
(define-test (length (eval-all '(queens 6))) 4)
(define-test (length (eval-all '(queens 7))) 40)
(define-test (length (eval-all '(queens 8))) 92)
