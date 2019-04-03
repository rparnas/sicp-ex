#|

Exercise 3.31: The internal procedure
"accept-action-procedure!" defined in "make-wire" specifies
that when a new action procedure is added to a wire, the
procedure is immediately run. Explain why this
initialization is necessary. In particular, trace through
the half-adder example in the paragraphs above and say how
the system's response would differ if we had defined
"accept-action-procedure!" as

(define (accept-action-procedure! proc)
  (set! action-procedures
        (cons proc action-procedures)))

|#

#| Previous exercies |#
(load-ex "3.21") ; delete-queue!, insert-queue!, make-queue
(load-ex "3.28") ; or-gate
(load-ex "3.30") ; ripple-carry-adder

#| Code from book -- half-adder, full-adder, inverter, and-gate |#
(define (half-adder a b s c)
  (let ([d (make-wire)] [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(define (full-adder a b c-in sum c-out)
  (let ([s (make-wire)]
        [c1 (make-wire)]
        [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "logical-not" "invalid signal" s)]))
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ([new-value
           (logical-and (get-signal a1) (get-signal a2))])
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

#| Missing code from book -- logical-and |#
(define (logical-and s1 s2)
  (cond [(and (= s1 1) (= s2 1)) 1]
        [else 0]))

#| Code from book -- agenda |#
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda)
  (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time actio)
    (let ([q (make-queue)])
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ([rest (cdr segments)])
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ([segments (segments agenda)])
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ([q (segment-queue (first-segment agenda))])
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "first-agenda-item" "agenda is empty")
      (let ([first-seg (first-segment agenda)])
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

#| Code from book -- wires |#
(define (make-wire)
  (let ([signal-value 0]
        [action-procedures '()])
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal-value]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-procedure!]
            [else (error "wire" "unknown procedure" m)]))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ([first-item (first-agenda-item the-agenda)])
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;;; modified to write to a string instead
(define out '())
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (set! out
                   (append out (list (format "~a ~a New-value = ~a" 
                                             name 
                                             (current-time the-agenda) 
                                             (get-signal wire))))))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

#| Customization |#
(define (reset-agenda)
  (set! the-agenda (make-agenda)))
(define (reset-out)
  (set! out '()))

#| Answer 

accept-action-procedure! immediately runs the given procedure so that any wires
affected by the action will have the correct initial value. This makes sense
becase we are not really considering this to be changing the state of the
system. Instead we are just incrementally defining the system.

For example, if you don't immediately call the action procedure for an inverter,
and the signal going into the inverter is at its default 0, the signal out of
the inverter will be 1 forever until the signal going in is changed.

In the example case, the only thing that probe will pick up is:
"carry 11 New-value = 1"

|#

#| Tests -- infrastructure |#
(define-test (let ([input-1 (make-wire)]
                   [input-2 (make-wire)]
                   [sum (make-wire)]
                   [carry (make-wire)])
                (reset-agenda)
                (reset-out)
                (probe 'sum sum)
                (probe 'carry carry)
                (half-adder input-1 input-2 sum carry)
                (set-signal! input-1 1)
                (propagate)
                (set-signal! input-2 1)
                (propagate)
                out)
              '("sum 0 New-value = 0"
                "carry 0 New-value = 0"
                "sum 8 New-value = 1"
                "carry 11 New-value = 1"
                "sum 16 New-value = 0"))

;;; test to clean the environment before returning to the REPL
(define-test (begin (reset-agenda)
                    (reset-out)
                    0)
             0)
              