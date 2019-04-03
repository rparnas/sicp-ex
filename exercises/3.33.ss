#|

Exercise 3.33: Using primitive multiplier, adder, and
constant constraints, define a procedure "averager" that
takes three connectors "a", "b", and "c" as inputs and
establishes the constraint that the value of "c" is the
average of the values of "a" and "b".

|#

#| Code from book |#
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me)]))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? resuest 'I-lost-my-value) (process-forget-value)]
          [else (error "adder" "unknown request" request)]))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond [(or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me)]
          [(and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me)]
          [(and (has-value? product) (has-value? m1))
            (set-value! m2
                        (/ (get-value product)
                           (get-value m1))
                        me)]
          [(and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me)]))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else (error "multiplier" "unknown request" request)]))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "constant" "unknown request" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;;; modified to write strings to a list
(define outs '())
(define (out line) (set! outs (append outs (list line))))
(define (flush-outs)
  (let ([ret outs])
    (set! outs '())
    ret))
(define (probe name connector)
  (define (print-probe value)
    (out (format "~a = ~a" name value)))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [error "probe" "unknown request" request]))
  (connect connector me)
  me)

(define (make-connector)
  (let ([value #f]
        [informant #f]
        [constraints '()])
    (define (set-my-value newval setter)
      (cond [(not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints)]
            [(not (= value newval))
             (error "connector" "constradition" (list value newval))]
            [else 'ignored]))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin
            (set! informant #f)
            (for-each-except retractor
                             inform-about-no-value
                             constraints))
          'ignored))
    (define (connect new-contraint)
      (if (not (memq new-contraint constraints))
          (set! constraints
                (cons new-contraint constraints))
          (void))
      (if (has-value? me)
          (inform-about-value new-contraint)
          (void))
      'done)
    (define (me request)
      (cond [(eq? request 'has-value?)
             (if informant #t #f)]
            [(eq? request 'value)
             value]
            [(eq? request 'set-value!)
             set-my-value]
            [(eq? request 'forget)
             forget-my-value]
            [(eq? request 'connect)
             connect]
            [else 
             (error "connector" "unknown operation" request)]))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond [(null? items) 'done]
          [(eq? (car items) exception)
           (loop (cdr items))]
          [else (procedure (car items))
                (loop (cdr items))]))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

#| Answer |#
 (define (averager a b c) 
   (let ((u (make-connector)) 
         (v (make-connector))) 
     (adder a b u) 
     (multiplier c v u) 
     (constant 2 v) 
     'ok))

#| Tests |#
(define-test
  (let ([a (make-connector)]
        [b (make-connector)]
        [c (make-connector)])
    (averager a b c)
    (probe "a" a)
    (probe "b" b)
    (probe "c" c)
    (set-value! a 4 'user)
    (set-value! b 6 'user)
    (flush-outs))
  '("a = 4" 
    "b = 6" 
    "c = 5"))

(define-test
  (let ([a (make-connector)]
        [b (make-connector)]
        [c (make-connector)])
    (averager a b c)
    (probe "a" a)
    (probe "b" b)
    (probe "c" c)
    (set-value! a 4 'user)
    (set-value! c 6 'user)
    (flush-outs))
  '("a = 4" 
    "c = 6" 
    "b = 8"))
