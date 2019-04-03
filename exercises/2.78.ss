#|

Exercise 2.78: The internal procedures in the
"scheme-number" package are essentially nothing more than
calls to the primitive procedures "+", "-", etc. It was not
possible to use the primitives of the language directly
because our type-tag system requires that each data object
have a type attached to it. In fact, however, all Lisp
implementations do have a type system, which they use
internally. Primitive predicates such as "symbol?" and
"number?" determine whether data objects have particular
types. Modify the definitions of "type-tag", "contents", and
"attach-tag" from Section 2.4.2 so that our generic system
takes advantage of Scheme's internal type system. That is to
say, the system should work as before except that ordinary
numbers should be represented simply as Scheme numbers
rather than as pairs whose "car" is the symbol
"scheme-number".

|#

#| Answer |#
(load-ex "2.77")

(define (attach-tag type-tag contents)
  (cond [(eq? 'scheme-number type-tag) contents]
        [else (cons type-tag contents)]))

(define (type-tag datum)
  (cond [(pair? datum) (car datum)]
        [(number? datum) 'scheme-number]
        [else (error "type-tag" "bad datum" datum)]))

(define (contents datum)
  (cond [(pair? datum) (cdr datum)]
        [(number? datum) datum]
        [else (error "contents" "bad datum" datum)]))

#| Tests |#
(define-test (make-scheme-number 5)
             5)
(define-test (type-tag (make-scheme-number 5)) 
             'scheme-number)
(define-test (contents (make-scheme-number 5)) 
             5)
(define-test (add (make-scheme-number 2) (make-scheme-number 2))
             4)
(define-test (type-tag (add (make-scheme-number 2) (make-scheme-number 2)))
             'scheme-number)
(define-test (contents (add (make-scheme-number 2) (make-scheme-number 2)))
             4)
