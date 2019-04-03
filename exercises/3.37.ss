#|

Exercise 3.37: The "celsius-fahrenheit-converter" procedure
is cumbersome when compared with a more expression-oriented
style of definition, such as

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

Here "c+", "c*", etc. are the "constraint" versions of the
arithmetic operations. For example, "c+" takes two
connectors as arguments and returns a connector that is
related to these by an adder constraint:

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

Define analogous procedures "c-", "c*", "c/", and "cv"
(constant value) that enable us to define compound
constraints as in the converter example above.

|#

(load-ex "3.35")

#| Code from book |#
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

#| Answer |#
(define (c- x y)
  (let ([ret (make-connector)])
    (adder ret y x)
    ret))

(define (c* x y)
  (let ([ret (make-connector)])
    (multiplier x y ret)
    ret))

(define (c/ x y)
  (let ([ret (make-connector)])
    (multiplier y ret x)
    ret))

(define (cv x) 
  (let ([ret (make-connector)])
    (constant x ret)
    ret))

#| Tests -- infrastructure |#
(define (op-test op . setters)
  (let* ([a (make-connector)]
         [b (make-connector)]
         [c (op a b)])
    (probe "a" a)
    (probe "b" b)
    (probe "c" c)
    (for-each (lambda (x)
                (let* ([sym (car x)]
                       [value (cdr x)]
                       [connector (cond [(eq? sym 'a) a]
                                        [(eq? sym 'b) b]
                                        [(eq? sym 'c) c])])
                (set-value! connector value 'user)))
              setters)
    (flush-outs)))

(define-test 
  (let ([x (cv 15)])
    (get-value x))
  15)

(define-test (op-test c+ '(a . 2) '(b . 2)) '("a = 2" "b = 2" "c = 4"))
(define-test (op-test c+ '(a . 2) '(c . 2)) '("a = 2" "c = 2" "b = 0"))
(define-test (op-test c+ '(b . 2) '(c . 2)) '("b = 2" "c = 2" "a = 0"))
(define-test (op-test c- '(a . 2) '(b . 2)) '("a = 2" "b = 2" "c = 0"))
(define-test (op-test c- '(a . 2) '(c . 2)) '("a = 2" "c = 2" "b = 0"))
(define-test (op-test c- '(b . 2) '(c . 2)) '("b = 2" "c = 2" "a = 4"))
(define-test (op-test c* '(a . 2) '(b . 3)) '("a = 2" "b = 3" "c = 6"))
(define-test (op-test c* '(a . 2) '(c . 4)) '("a = 2" "c = 4" "b = 2"))
(define-test (op-test c* '(b . 2) '(c . 4)) '("b = 2" "c = 4" "a = 2"))
(define-test (op-test c/ '(a . 2) '(b . 2)) '("a = 2" "b = 2" "c = 1"))
(define-test (op-test c/ '(a . 8) '(c . 4)) '("a = 8" "c = 4" "b = 2"))
(define-test (op-test c/ '(b . 2) '(c . 1)) '("b = 2" "c = 1" "a = 2"))

#| Tests |#
(define-test
  (let* ([c (make-connector)]
         [f (celsius-fahrenheit-converter c)])
    (probe "c" c)
    (probe "f" f)
    (set-value! c 100 'user)
    (flush-outs))
    '("c = 100" "f = 212"))

(define-test
  (let* ([c (make-connector)]
         [f (celsius-fahrenheit-converter c)])
    (probe "c" c)
    (probe "f" f)
    (set-value! f 50 'user)
    (flush-outs))
    '("f = 50" "c = 10"))
