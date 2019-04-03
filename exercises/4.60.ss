#|

Exercise 4.60: By giving the query

(lives-near ?person (Hacker Alyssa P))

Alyssa P. Hacker is able to find people who live near her,
with whom she can ride to work. On the other hand, when she
tries to find all pairs of people who live near each other
by querying

(lives-near ?person-1 ?person-2)

she notices that each pair of people who live near each
other is listed twice; for example,

(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

Why does this happen? Is there a way to find a list of
people who live near each other, in which each pair appears
only once? Explain.

|#

(load-ex "4.59")

#| Answer

Lives-near creates a stream of every person paired with every other person. This
stream includes pairs which are the same except the order is reversed.

To prevent duplicates you could create something like "and" except it considers
each pair a set which should appear only once. Or you can create some way to
order keys and filter via lisp-value to only include pairs where the first key
is larger.

|#

(define (name-gt name-a name-b)
  (define (name->integer-list name)
    (map char->integer 
         (string->list (fold-right (lambda (a b) (string-append a " " b)) 
                                   "" 
                                   (map symbol->string name)))))
  (define (gt ls0 ls1)
    (cond [(and (null? ls0) (null? ls1)) (error "name-gt" "key collision")]
          [(null? ls0) #f]
          [(null? ls1) #t]
          [(= (car ls0) (car ls1)) (gt (cdr ls0) (cdr ls1))]
          [else (< (car ls0) (car ls1))]))
  (let ([a (name->integer-list name-a)]
        [b (name->integer-list name-b)])
    (gt a b)))

#| Tests |#
(define-test (length (do-query test-db 
  '(and (address ?p1 . ?rest1)
        (address ?p2 . ?rest2)
        (not (same ?p1 ?p2))
        (lisp-value name-gt ?p1 ?p2))))
  36) ; sum of 1..(n-1)

(define-test (do-query test-db
  '(and (lives-near ?p1 ?p2)
        (lisp-value name-gt ?p1 ?p2)))
  '((and (lives-near (Bitdiddle Ben) (Reasoner Louis))
         (lisp-value name-gt (Bitdiddle Ben) (Reasoner Louis)))
    (and (lives-near (Fect Cy D) (Hacker Alyssa P))
         (lisp-value name-gt (Fect Cy D) (Hacker Alyssa P)))
    (and (lives-near (Aull DeWitt) (Bitdiddle Ben))
         (lisp-value name-gt (Aull DeWitt) (Bitdiddle Ben)))
    (and (lives-near (Aull DeWitt) (Reasoner Louis))
         (lisp-value name-gt (Aull DeWitt) (Reasoner Louis)))))