#|

Exercise 4.48: Extend the grammar given above to handle more
complex sentences. For example, you could extend noun
phrases and verb phrases to include adjectives and adverbs,
or you could handle compound sentences.

|#

(load-ex "4.45")

#| Answer |#
(define setup-environment-445 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-445)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))
    (add '(define adjectives '(adjective red green blue nice mean)))
    (add '(define adverbs '(adverb quickly slowly)))

    ;;; extension --- (article) (0 or more adjectives) (noun)
    (add '(define (parse-simple-noun-phrase)
      (define (rest)
        (amb (list (parse-word nouns))
             (append (list (parse-word adjectives))
                     (rest))))
      (append (list 'simple-noun-phrase 
                    (parse-word articles))
              (rest))))

    ;;; extension -- (0 or 1 adverbs) (verb) (0 or more prepositions)
    ;;;   natural multiple adverbs seems like a lot of work as they are usually
    ;;;   used with punctuation or conjunctions.
    (add '(define (parse-verb-phrase)
            (define (maybe-extend verb-phrase)
              (amb verb-phrase
                   (maybe-extend (list 'verb-phrase
                                       verb-phrase
                                       (parse-prepositional-phrase)))))
            (maybe-extend (amb (parse-word verbs)
                               (list 'verb-phrase
                                     (parse-word adverbs)
                                     (parse-word verbs))))))
    ;;; return
    env))

#| Tests |#
(define-test (map get-knowledge (eval-all '(parse 
  '(the mean red cat sleeps in the class with the student))))
  '(("the mean red cat sleeps"
     "they sleeps with the student"
     "they sleeps in the class")
    ("the mean red cat sleeps"
      "they sleeps in the class"
      "the class is the class with the student")))

;;; did not yet update get-knowledge for adverbs
(define-test (eval-all '(parse
  '(the professor quickly lectures)))
  '((sentence
    (simple-noun-phrase (article the) (noun professor))
    (verb-phrase (adverb quickly) (verb lectures)))))