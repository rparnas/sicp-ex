#|

Exercise 4.49: Alyssa P. Hacker is more interested in
generating interesting sentences than in parsing them. She
reasons that by simply changing the procedure "parse-word"
so that it ignores the "input sentence" and instead always
succeeds and generates an appropriate word, we can use the
programs we had built for parsing to do generation instead.
Implement Alyssa's idea, and show the first half-dozen or so
sentences generated.

|#

(load-ex "4.48")
(no-regression)

#| Answer 

In generating all possible sentences, you'd have to think hard about how you
want to explore the search space and what kinds of random sentences you want.

Some easy hacks to find more meaningful results could be to restrict to
sentences of a certain word count or syllable count.

|#

(define setup-environment-448 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-448)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))
    ;;; changes
    (add '(define made-up '()))

    ;;; use unparsed to mean "how long of a random sentence do you want"
    ;;; this provides more interesting results.
    (add '(define (parse-word word-list)
            ; (require (not (null? *unparsed*)))
            (define (iter words)
              (if (null? words)
                  (amb)
                  (amb (car words)
                       (iter (cdr words)))))
            (let ([word (iter (cdr word-list))])
              (set! made-up (append made-up (list word)))
              ; (set! *unparsed* (cdr *unparsed*))
              (list (car word-list) word))))
    ;;; return
    env))

#| Tests |#

(define-test (eval-n '(begin (parse '()) made-up) 6)
             '((the student studies) 
               (the student studies for the student)
               (the student studies for the student for the student)
               (the student studies for the student for the student for the student)
               (the student studies for the student for the student for the student for the student)
               (the student studies for the student for the student for the student for the student for the student)))

;;;; result if we restrict to a certain word count
#|(define-test (eval-all '(begin (parse '(one two three)) made-up))
 '((the student studies) 
   (the student lectures)
   (the student eats) 
   (the student sleeps)
   (the professor studies) 
   (the professor lectures)
   (the professor eats) 
   (the professor sleeps)
   (the cat studies) 
   (the cat lectures) 
   (the cat eats)
   (the cat sleeps) 
   (the class studies) 
   (the class lectures)
   (the class eats) 
   (the class sleeps) 
   (a student studies)
   (a student lectures) 
   (a student eats) 
   (a student sleeps)
   (a professor studies) 
   (a professor lectures)
   (a professor eats) 
   (a professor sleeps) 
   (a cat studies)
   (a cat lectures) 
   (a cat eats) 
   (a cat sleeps)
   (a class studies) 
   (a class lectures) 
   (a class eats)
   (a class sleeps)))|#
