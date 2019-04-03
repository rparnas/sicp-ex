#|

Exercise 4.47: Louis Reasoner suggests that, since a verb
phrase is either a verb or a verb phrase followed by a
prepositional phrase, it would be much more straightforward
to define the procedure "parse-verb-phrase" as follows (and
similarly for noun phrases):

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

Does this work? Does the program's behavior change if we
interchange the order of expressions in the "amb"?

|#

#| Answer 

This does not work. When parse-verb-phrase is called and the first word is not a
verb, parse-verb-phrase is called again to generate an alternative possibility
for the ambiguous value leading to an infinite loop.

If you flip the order of the arguments to amb, you get another infinite loop
because the first possibility for the ambiguous value involves a recursive call.

(define (parse-verb-phrase)
  (amb (list 'verb-phrase
             (parse-verb-phrase)
             . . .

|#
