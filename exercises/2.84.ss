#|

Exercise 2.84: Using the "raise" operation of Exercise 2.83,
modify the "apply-generic" procedure so that it coerces its
arguments to have the same type by the method of successive
raising, as discussed in this section. You will need to
devise a way to test which of two types is higher in the
tower. Do this in a manner that is "compatible" with the
rest of the system and will not lead to problems in adding
new levels to the tower.

|#

(load-ex "2.83")

#| Answer |#
(define (apply-generic op . args)
  ; Call upon failure
    (define (fail)
      (error "apply-generic" "no method for" (list op (map type-tag args))))
  ; returns true if the given type can be raised
  (define (can-raise? type)
    (let ([pos (memq type tower)])
      (and pos (> (length pos) 1))))
  ; returns true if type a is less than type b
  (define (type-lt a b)
    (let ([pos-a (memq a tower)]
          [pos-b (memq b tower)])
      (cond [(not pos-a) #f]
            [(not pos-b) (if pos-a #t #f)]
            [else (> (length pos-a) (length pos-b))])))
  ; finds the least type in the list of types
  (define (least-type types)
    (define (iter result types)
      (cond [(null? types) result]
            [(type-lt result (car types))
             (iter result (cdr types))]
            [else
             (iter (car types) (cdr types))]))
    (iter (car types) (cdr types)))
  ; raises one instance of an argument with the least type
  (define (raise-least args)
    (define (iter result args type)
      (if (eq? type (type-tag (car args)))
          (append result 
                  (list (raise (car args)))
                  (cdr args))
          (iter (append result
                        (list (car args)))
                (cdr args)
                type)))
    (let ([least (least-type (map type-tag args))])
      (if (can-raise? least)
          (iter '() args least)
          #f)))
  ; iterate through successive raises
  (define (iter op args)
    (let ([proc (get op (map type-tag args))])
      (if proc
          (apply proc (map contents args))
          (let ([raised-args (raise-least args)])
            (if raised-args
                (iter op raised-args)
                (fail))))))
  ; body
  (iter op args))

#| Tests |#

(define-test (add (make-integer 2) (make-integer 2))
             '(integer . 4))
(define-test (add (make-integer 2) (make-rational 1 2))
             '(rational 5 . 2))
(define-test (add (make-integer 2) (make-real (/ 1 3)))
             '(real . 7/3))
(define-test (add (make-integer 2) (make-complex-from-real-imag (/ 1 3) (/ 1 3)))
             '(complex rectangular 7/3 . 1/3))

; > (apply-generic 'sad (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
; Exception in apply-generic: no method for with irritant (sad (complex complex))
; Type (debug) to enter the debugger.
