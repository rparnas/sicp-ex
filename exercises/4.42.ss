#|

Exercise 4.42: Solve the following "Liars" puzzle (from
Phillips 1934):

Five schoolgirls sat for an examination. Their parents---so
they thought---showed an undue degree of interest in the
result. They therefore agreed that, in writing home about
the examination, each girl should make one true statement
and one untrue one. The following are the relevant passages
from their letters:

* Betty: "Kitty was second in the examination. I was only
third."

* Ethel: "You'll be glad to hear that I was on top. Joan was
2nd."

* Joan: "I was third, and poor old Ethel was bottom."

* Kitty: "I came out second. Mary was only fourth."

* Mary: "I was fourth. Top place was taken by Betty."

What in fact was the order in which the five girls were
placed?

|#

(load-ex "4.40") ; especially for permutations

#| Answer |#

(define setup-environment-440 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-440)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))

    ;;; for try 1
    (add '(define (require-xor a b)
            (require (or (and a (not b))
                         (and b (not a))))))
    ;;; return
    env))

#| Tests |#
(define-test (eval-all 
  '((lambda ()
     (let* ([seats (permutations '(1 2 3 4 5))]
            [betty (car seats)]
            [ethel (car (cdr seats))]
            [joan (car (cdr (cdr seats)))]
            [kitty (car (cdr (cdr (cdr seats))))]
            [mary (car (cdr (cdr (cdr (cdr seats)))))])
       (require-xor (= kitty 2) (= betty 3))
       (require-xor (= ethel 1) (= joan 2))
       (require-xor (= joan 3) (= ethel 5))
       (require-xor (= kitty 2) (= mary 4))
       (require-xor (= mary 4) (= betty 1))
       (list (list 'betty betty)
             (list 'ethel ethel)
             (list 'joan joan)
             (list 'kitty kitty)
             (list 'mary mary))))))
  '(((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))))
