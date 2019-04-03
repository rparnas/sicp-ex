#|

Exercise 4.50: Implement a new special form "ramb" that is
like "amb" except that it searches alternatives in a random
order, rather than from left to right. Show how this can
help with Alyssa's problem in Exercise 4.49.

|#

(load-ex "4.49")
(no-regression) ; shouldn't be required but must prevent pre-4.49 tests from running.

#| Answer

Only using ramb in parse-word doesn't help much as you still get stuck in traps,
they are just a random trap each time.

Replacing amb everywhere in the langauge processor tends toward very long
results which also seem trapped.

I'm not sure if the correct way forward is arbitrary restrictions such as word
length or something more complex.

|#

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (remove-at ls index)
  (define (iter result ls i)
    (cond [(null? ls) (error "remove-at" "inavlid index" index)]
          [(= i 0) (append result (cdr ls))]
          [else (iter (append result (list (car ls))) (cdr ls) (- i 1))]))
  (iter '() ls index))

(define (get-random-item-and-rest ls)
  (let* ([index (random (length ls))]
         [item (list-ref ls index)]
         [rest (remove-at ls index)])
    (cons item rest)))

(define (analyze-ramb exp)
  (let ([cprocs (map analyze (ramb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let* ([choice-and-rest (get-random-item-and-rest choices)]
                   [choice (car choice-and-rest)]
                   [rest (cdr choice-and-rest)])
              (choice env
                      succeed
                      (lambda ()
                        (try-next rest))))))
      (try-next cprocs))))

(define analyze-449 analyze)
(set! analyze (lambda (exp)
  (cond [(ramb? exp) (analyze-ramb exp)]
        [else (analyze-449 exp)])))

(define setup-environment-449 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-449)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))
    ;;; ramb-ify parse-word from 4.49
    (add '(define (parse-word word-list)
            (define (iter words)
              (if (null? words)
                  (ramb)
                  (ramb (car words)
                       (iter (cdr words)))))
            (let ([word (iter (cdr word-list))])
              (set! made-up (append made-up (list word)))
              (list (car word-list) word))))
    ;;; ramb-ify parse-simple-noun-phrase and parse-verb-phrase from 4.48
    (add '(define (parse-simple-noun-phrase)
      (define (rest)
        (ramb (list (parse-word nouns))
             (append (list (parse-word adjectives))
                     (rest))))
      (append (list 'simple-noun-phrase 
                    (parse-word articles))
              (rest))))
    (add '(define (parse-verb-phrase)
            (define (maybe-extend verb-phrase)
              (ramb verb-phrase
                   (maybe-extend (list 'verb-phrase
                                       verb-phrase
                                       (parse-prepositional-phrase)))))
            (maybe-extend (ramb (parse-word verbs)
                               (list 'verb-phrase
                                     (parse-word adverbs)
                                     (parse-word verbs))))))
    ;;; ramb-ify parse-noun-phrase from 4.45 (skip parse-verb-phrase it was already replaced in 4.48)
    (add '(define (parse-noun-phrase)
        (define (maybe-extend noun-phrase)
          (ramb noun-phrase
               (maybe-extend (list 'noun-phrase
                                   noun-phrase
                                   (parse-prepositional-phrase)))))
        (maybe-extend (parse-simple-noun-phrase))))
    ;;; return
    env))

#| Tests 

> (eval-all '(ramb 1 2 3 4))
(2 3 1 4)
> (eval-all '(ramb 1 2 3 4))
(1 4 3 2)

;;;; stuck in random traps
> (eval-n '(begin (parse '()) made-up) 6)
((a student eats) (a student eats by the cat)
  (a student eats by the cat by the student)
  (a student eats by the cat by the student for the professor)
  (a student eats by the cat by the student for the professor to a professor)
  (a student eats by the cat by the student for the professor to a professor to a professor))
> (eval-n '(begin (parse '()) made-up) 6)
((the class lectures) (the class lectures for a student)
  (the class lectures for a student for the student)
  (the class lectures for a student for the student for a student)
  (the class lectures for a student for the student for a student for the student)
  (the class lectures for a student for the student for a student for the student for a professor))

;;;; additional randomness
> (eval-n '(begin (parse '()) made-up) 6)
((the student studies with a mean student in the red red blue
  cat in the green blue student to a student for a red student
  for a red student for a red student for a green student for
  a red green red red green red mean professor for the student
  in the nice green green professor for the green student by a
  blue red nice student with the green red cat for a green
  student for a green blue green professor for a red blue
  student with a student to the student for a professor for
  the red red red nice professor to a blue red green student
  for the professor)
. . .

|#
