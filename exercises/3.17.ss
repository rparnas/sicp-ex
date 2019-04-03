#|

Exercise 3.17: Devise a correct version of the "count-pairs"
procedure of Exercise 3.16 that returns the number of
distinct pairs in any structure. (Hint: Traverse the
structure, maintaining an auxiliary data structure that is
used to keep track of which pairs have already been
counted.)

|#

#| Answer |#
(define (count-pairs x)
  (define visited '())
  (define (has-visited x)
    (ormap (lambda (item) (eq? item x)) visited))
  (define (iter x)
    (if (or (not (pair? x)) (has-visited x))
        0
        (begin
          (set! visited (append visited (list x)))
          (+ (iter (car x))
             (iter (cdr x))
             1))))
  (iter x))

#| Tests |#
(define-test (let* ([x (cons 1 (cons 2 (cons 3 '())))])
               (count-pairs x))
             3)

(define-test (let* ([right (cons 2 '())]
                    [left (cons 1 right)]
                    [x (cons left right)])
               (count-pairs x))
             3)

(define-test (let* ([bottom (cons 1 '())]
                    [middle (cons bottom bottom)]
                    [x (cons middle middle)])
               (count-pairs x))
             3)

(define-test (begin
              (define x (cons 1 (cons 2 (cons 3 '()))))
              (append! x x)
              (count-pairs x))
             3)