#|

Exercise 4.51: Implement a new kind of assignment called
"permanent-set!" that is not undone upon failure. For
example, we can choose two distinct elements from a list and
count the number of trials required to make a successful
choice as follows:

(define count 0)
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))
 ;;; Starting a new problem 
 ;;; Amb-Eval value: 
 (a b 2) 
 ;;; Amb-Eval input: 
try-again
 ;;; Amb-Eval value: 
 (a c 3) 

What values would have been displayed if we had used "set!"
here rather than "permanent-set!" ?

|#

(load-ex "4.50")
(no-regression)

#| Answer 

If set! is used, (a b 1) and (a c 1) is displayed. The trailing value will
always be one because every time we declare an amb failure, we  backtrack past
it and undo it.

|#

(define (passignment? exp) (tagged-list? exp 'permanent-set!))
(define (passignment-variable exp) (cadr exp))
(define (passignment-value exp) (caddr exp))

(define (analyze-passignment exp)
  (let ([var (passignment-variable exp)]
        [vproc (analyze (passignment-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ([old-value (lookup-variable-value var env)])
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            ; (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

(define analyze-450 analyze)
(set! analyze (lambda (exp)
  (cond [(passignment? exp) (analyze-passignment exp)]
        [else (analyze-450 exp)])))

#| Tests |#

;;; NOTE: My version of "an-element-of" is "one-from-set" from 4.40
(define-test (eval-n
  '(begin
     (define count 0)
       (let ((x (one-from-set '(a b c)))
             (y (one-from-set '(a b c))))
         (permanent-set! count (+ count 1))
         (require (not (eq? x y)))
         (list x y count)))
  2)
  '((a b 2) (a c 3)))

(define-test (eval-all
  '(begin
     (define count 0)
       (let ((x (one-from-set '(a b c)))
             (y (one-from-set '(a b c))))
         (set! count (+ count 1))
         (require (not (eq? x y)))
         (list x y count))))
  '((a b 1) (a c 1) (b a 1) (b c 1) (c a 1) (c b 1)))