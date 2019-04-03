#|

Exercise 5.3: Design a machine to compute square roots using
Newton's method, as described in Section 1.1.7:

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

Begin by assuming that "good-enough?" and "improve"
operations are available as primitives. Then show how to
expand these in terms of arithmetic operations. Describe
each version of the "sqrt" machine design by drawing a
data-path diagram and writing a controller definition in the
register-machine language.

|#

(load-ex "5.2")

#| Answer -- skipped diagrams |#
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-prim-machine) 
  (make-machine
    '(guess x)
    (list (list 'good-enough? (lambda (x guess)
                                (< (abs (- (square guess) x)) 0.001)))
          (list 'improve-guess (lambda (x guess)
                                 (average guess (/ x guess)))))
    '((assign guess (const 1.0))
      iter
      (test (op good-enough?) (reg x) (reg guess))
      (branch (label done))
      (assign guess (op improve-guess) (reg x) (reg guess))
      (goto (label iter))
      done)))

(define (sqrt-machine)
  (make-machine
    '(guess x temp)
    (list (list 'square (lambda (x) (* x x)))
          (list '- -)
          (list 'abs abs)
          (list '< <)
          (list '/ /)
          (list 'average average))
    '((assign guess (const 1.0))
      iter
      ;;; (if (good-enough? guess) guess...
      (assign temp (op square) (reg guess))
      (assign temp (op -) (reg temp) (reg x))
      (assign temp (op abs) (reg temp))
      (test (op <) (reg temp) (const 0.001))
      (branch (label done))
      ;;; (sqrt-iter (improve guess))
      (assign temp (op /) (reg x) (reg guess))
      (assign guess (op average) (reg guess) (reg temp))
      (goto (label iter))
      done)))

#| Tests |#
(define-test (let ([m (sqrt-prim-machine)]) (begin
  (set-register-contents! m 'x 2)
  (start m)
  (let ([result (get-register-contents m 'guess)])
    (and (> result 1.4) (< result 1.5)))))
  #t)

(define-test (let ([m (sqrt-machine)]) (begin
  (set-register-contents! m 'x 2)
  (start m)
  (let ([result (get-register-contents m 'guess)])
    (and (> result 1.4) (< result 1.5)))))
  #t)
