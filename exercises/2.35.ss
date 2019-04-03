#|

Exercise 2.35: Redefine "count-leaves" from Section 2.2.2 as
an accumulation:

(define (count-leaves t)
  (accumulate <??> <??> (map <??> <??>)))

|#

#| Code from book |#

(define (count-leaves x)
  (cond [(null? x) 0]
        [(not (pair? x)) 1]
        [else (+ (count-leaves (car x)) 
                 (count-leaves (cdr x)))]))

(define (enumerate-tree tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree)) 
                      (enumerate-tree (cdr tree)))]))

#| Answer |#
(load-ex "2.33")
(define (count-leaves-improved t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

#| Tests |#
(define t (list (list 1 2) (list 1 2 3) 1))
(define-test (count-leaves t) 6)
(define-test (count-leaves-improved (list (list 1 2) (list 1 2 3) 1)) 6)
