#|

Exercise 4.45: With the grammar given above, the following
sentence can be parsed in five different ways: "The
professor lectures to the student in the class with the
cat." Give the five parses and explain the differences in
shades of meaning among them.

|#

(load-ex "4.44")

#| Code from book |#
(define setup-environment-444 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-444)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))
    (add '(define nouns '(noun student professor cat class)))
    (add '(define verbs '(verb studies lectures eats sleeps)))
    (add '(define articles '(article the a)))
    (add '(define prepositions '(prep for to in by with)))
    (add '(define (parse-sentence)
            (list 'sentence
                  (parse-noun-phrase)
                  (parse-verb-phrase))))
    (add '(define (parse-noun-phrase)
            (define (maybe-extend noun-phrase)
              (amb noun-phrase
                   (maybe-extend (list 'noun-phrase
                                       noun-phrase
                                       (parse-prepositional-phrase)))))
            (maybe-extend (parse-simple-noun-phrase))))
    (add '(define (parse-prepositional-phrase)
            (list 'prep-phrase
              (parse-word prepositions)
              (parse-noun-phrase))))
    (add '(define (parse-simple-noun-phrase)
            (list 'simple-noun-phrase
            (parse-word articles)
            (parse-word nouns))))
    (add '(define (parse-verb-phrase)
            (define (maybe-extend verb-phrase)
              (amb verb-phrase
                   (maybe-extend (list 'verb-phrase
                                       verb-phrase
                                       (parse-prepositional-phrase)))))
            (maybe-extend (parse-word verbs))))
    (add '(define (parse-word word-list)
            (require (not (null? *unparsed*)))
            (require (memq (car *unparsed*) (cdr word-list)))
            (let ([found-word (car *unparsed*)])
              (set! *unparsed* (cdr *unparsed*))
              (list (car word-list) found-word))))
    (add '(define *unparsed* '()))
    (add '(define (parse input)
            (set! *unparsed* input)
            (let ([sent (parse-sentence)])
              (require (null? *unparsed*))
              sent)))
    ;;; return
    env))

#| Answer 

The professor lectures to the student in the class with the cat.

Running the analyzer below (to make things easier to read) we get:
  (("the professor lectures"
     "they lectures with the cat"
     "they lectures in the class"
     "they lectures to the student")
    ("the professor lectures"
      "they lectures in the class"
      "they lectures to the student"
      "the class is the class with the cat")
    ("the professor lectures"
      "they lectures with the cat"
      "they lectures to the student"
      "the student is the student in the class")
    ("the professor lectures"
      "they lectures to the student"
      "the student is the student with the cat"
      "the student is the student in the class")
    ("the professor lectures"
      "they lectures to the student"
      "the student is the student in the class"
      "the class is the class with the cat"))

In every interpretation we know:
  * the professor lectures
  * they lectures to the student

The ambiguities are:
* What does "in the class" describe?
  - they lectures in the class
  - the student is the student in the class
* What does "with the cat" describe?
  - they lectures with the cat
  - the class is the class with the cat
  - the student is the student with the cat

Our system returns:
1 | they lectures in the class              | they lectures with the cat
2 | they lectures in the class              | the class is the class with the cat
x | they lectures in the class              | the student is the student with the cat
3 | the student is the student in the class | they lectures with the cat
5 | the student is the student in the class | the class is the class with the cat
4 | the student is the student in the class | the student is the student with the cat

One "possibility" is impossible. If we assume that "to the student" and "in the
class" just describe how the professor is lecturing, we can't assume that "with
the cat" describes the student. I.e. we're past the part of the sentence that
was talking about the student.

|#

(define (type x) (car x))
(define (contents x) (cdr x))

(define (stringify ls)
    (if (null? ls)
        ""
        (let ([item (symbol->string (cadar ls))])
          (if (null? (cdr ls))
              item
              (string-append item " " (stringify (cdr ls)))))))

(define (has-structure? x expected-types)
  (let ([types (cons (type x) (map type (contents x)))])
    (and (= (length x) (length expected-types))
         (andmap (lambda (t valid) (memq t valid))
                 types
                 expected-types))))

(define (is-type? x types)
  (memq (type x) types))

(define (get-knowledge x)
  (cond [(is-type? x '(prep)) (list)]
        [(is-type? x '(verb)) (list)]
        [(is-type? x '(simple-noun-phrase)) (list)]
        [(has-structure? x '((noun-phrase) (simple-noun-phrase noun-phrase) (prep prep-phrase)))
         (let ([noun-string (find-noun (cadr x))]
               [prep-string (find-prep  (caddr x))])
           (append (list (string-append noun-string " is " noun-string " " prep-string))
                   (get-knowledge (cadr x))
                   (get-knowledge (caddr x))))]
        [(has-structure? x '((prep-phrase) (prep prep-phrase) (simple-noun-phrase noun-phrase)))
         (append (get-knowledge (cadr x))
                 (get-knowledge (caddr x)))]
        [(has-structure? x '((sentence) (simple-noun-phrase noun-phrase) (verb verb-phrase)))
         (let ([noun-string (find-noun (cadr x))]
               [verb-string (find-verb  (caddr x))])
           (append (list (string-append noun-string " " verb-string))
                   (get-knowledge (cadr x))
                   (get-knowledge (caddr x))))]
        [(has-structure? x '((verb-phrase) (verb verb-phrase) (prep-phrase)))
         (let* ([verb-string (find-verb (cadr x))]
                [prep-string (find-prep (caddr x))])
           (append (list (string-append "they " verb-string " " prep-string))
                   (get-knowledge (cadr x))
                   (get-knowledge (caddr x))))]
        [else (error "get-knowledge" "not supported" x)]))

(define (find-noun x)
  (cond [(is-type? x '(simple-noun-phrase)) (stringify (cdr x))]
        [(is-type? x '(noun-phrase)) (find-noun (cadr x))]
        [else (error "find-noun" "invalid type" x)]))

(define (find-prep x)
  (cond [(is-type? x '(prep)) (symbol->string (cadr x))]
        [(has-structure? x '((prep-phrase) (prep) (simple-noun-phrase noun-phrase)))
         (let ([prep-string (find-prep (cadr x))]
               [noun-string (find-noun (caddr x))])
           (string-append prep-string " " noun-string))]
        [(is-type? x 'prep-phrase) (find-prep (cadr x))]
        [else (error "find-prep" "invalid type" x)]))

(define (find-verb x)
  (define (find-verb-child children)
    (cond [(null? children) (error "find-verb" "no verb child found" x)]
          [(is-type? (car children) '(verb verb-phrase)) (car children)]
          [else (find-verb-child (cdr children))]))
  (cond [(is-type? x '(verb)) (symbol->string (cadr x))]
        [(is-type? x '(verb-phrase)) (find-verb (find-verb-child (contents x)))]
        [else (error "find-verb" "invalid type" x)]))

#| Tests -- infrastructure |#
(define-test (eval-all '(parse '(the cat eats)))
  '((sentence
     (simple-noun-phrase (article the) (noun cat))
     (verb eats))))

(define-test (eval-all '(parse '(the student with the cat sleeps in the class)))
  '((sentence
     (noun-phrase
       (simple-noun-phrase (article the) (noun student))
       (prep-phrase
         (prep with)
         (simple-noun-phrase (article the) (noun cat))))
     (verb-phrase
       (verb sleeps)
       (prep-phrase
         (prep in)
         (simple-noun-phrase (article the) (noun class)))))))

(define-test (eval-all '(parse '(the professor lectures to the student with the cat)))
    '((sentence
       (simple-noun-phrase (article the) (noun professor))
       (verb-phrase
         (verb-phrase
           (verb lectures)
           (prep-phrase
             (prep to)
             (simple-noun-phrase (article the) (noun student))))
         (prep-phrase
           (prep with)
           (simple-noun-phrase (article the) (noun cat)))))
      (sentence
        (simple-noun-phrase (article the) (noun professor))
        (verb-phrase
          (verb lectures)
          (prep-phrase
            (prep to)
            (noun-phrase
              (simple-noun-phrase (article the) (noun student))
              (prep-phrase
                (prep with)
                (simple-noun-phrase (article the) (noun cat)))))))))

#| Tests |#
(define-test (eval-all '(parse '(the professor lectures to the student in the class with the cat)))
  '((sentence
     (simple-noun-phrase (article the) (noun professor))
     (verb-phrase
       (verb-phrase
         (verb-phrase
           (verb lectures)
           (prep-phrase
             (prep to)
             (simple-noun-phrase (article the) (noun student))))
         (prep-phrase
           (prep in)
           (simple-noun-phrase (article the) (noun class))))
       (prep-phrase
         (prep with)
         (simple-noun-phrase (article the) (noun cat)))))
  (sentence
    (simple-noun-phrase (article the) (noun professor))
    (verb-phrase
      (verb-phrase
        (verb lectures)
        (prep-phrase
          (prep to)
          (simple-noun-phrase (article the) (noun student))))
      (prep-phrase
        (prep in)
        (noun-phrase
          (simple-noun-phrase (article the) (noun class))
          (prep-phrase
            (prep with)
            (simple-noun-phrase (article the) (noun cat)))))))
  (sentence
    (simple-noun-phrase (article the) (noun professor))
    (verb-phrase
      (verb-phrase
        (verb lectures)
        (prep-phrase
          (prep to)
          (noun-phrase
            (simple-noun-phrase (article the) (noun student))
            (prep-phrase
              (prep in)
              (simple-noun-phrase (article the) (noun class))))))
      (prep-phrase
        (prep with)
        (simple-noun-phrase (article the) (noun cat)))))
  (sentence
    (simple-noun-phrase (article the) (noun professor))
    (verb-phrase
      (verb lectures)
      (prep-phrase
        (prep to)
        (noun-phrase
          (noun-phrase
            (simple-noun-phrase (article the) (noun student))
            (prep-phrase
              (prep in)
              (simple-noun-phrase (article the) (noun class))))
          (prep-phrase
            (prep with)
            (simple-noun-phrase (article the) (noun cat)))))))
  (sentence
    (simple-noun-phrase (article the) (noun professor))
    (verb-phrase
      (verb lectures)
      (prep-phrase
        (prep to)
        (noun-phrase
          (simple-noun-phrase (article the) (noun student))
          (prep-phrase
            (prep in)
            (noun-phrase
              (simple-noun-phrase (article the) (noun class))
              (prep-phrase
                (prep with)
                (simple-noun-phrase (article the) (noun cat)))))))))))

(define-test (map get-knowledge
                  (eval-all '(parse '(the professor lectures to the student in the class with the cat))))
             '(("the professor lectures"
                "they lectures with the cat"
                "they lectures in the class"
                "they lectures to the student")
               ("the professor lectures"
                 "they lectures in the class"
                 "they lectures to the student"
                 "the class is the class with the cat")
               ("the professor lectures"
                 "they lectures with the cat"
                 "they lectures to the student"
                 "the student is the student in the class")
               ("the professor lectures"
                 "they lectures to the student"
                 "the student is the student with the cat"
                 "the student is the student in the class")
               ("the professor lectures"
                 "they lectures to the student"
                 "the student is the student in the class"
                 "the class is the class with the cat")))
