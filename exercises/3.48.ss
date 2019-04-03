#|

Exercise 3.48: Explain in detail why the deadlock-avoidance
method described above, (i.e., the accounts are numbered,
and each process attempts to acquire the smaller-numbered
account first) avoids deadlock in the exchange problem.
Re-write "serialized-exchange" to incorporate this idea.
(You will also need to modify "make-account" so that each
account is created with a number, which can be accessed by
sending an appropriate message.)

|#

#| Code from book |#

;;; ! must be defined first to avoid bug where make-serializer
;;; calls the built-in make-mutex
(define (make-mutex)
  (let ([cell (list #f)])
    (define (the-mutex m)
      (cond [(eq? m 'acquire)
             (if (test-and-set! cell)
             (the-mutex 'acquire))] ; retry
            [(eq? m 'release) 
             (clear! cell)]))
    the-mutex))

(define (make-serializer)
  (let ([mutex (make-mutex)])
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;;; with-interrupts-disabled added
(define (clear! cell) 
  (with-interrupts-disabled
    (set-car! cell #f)))

;;; with-interrupts-disabled added
(define (test-and-set! cell)
  (with-interrupts-disabled
    (if (car cell)
        #t
        (begin (set-car! cell #t)
               #f))))

(define (exchange account1 account2)
  (let ([difference (- (account1 'balance)
                       (account2 'balance))])
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

#| Answer 

If one operation needs a range of ids that is a subset of the range needed by
another it can't deadlock. If the bigger range op goes first it blocks the
smaller range op before the small range op takes any resources, to the bigger
range op will finish. If the small range op goes first it blocks the bigger
range op, but the small range op will finish because the big range op has no
resources that it needs yet, or else the small range op would have asked for
them already.

  [a   ]
[b       ]

If there are many overlapping ranges of ids, if the operation which needs the
highest range of ids goes first it will finish because by definition it needs no
ids lower than those it has already grabbed. If some other operation goes first
it may be blocked by an operation needing a further right range of ids but that
righter-er operation will eventually finish and release its resources.

           [d     ]
       [c     ]
    [b    ]
[a    ]

|#

;;; id and serializer are considered immutable and safe to access without a
;;; serializer.
(define make-account-and-serializer
  (let* ([last-id -1]
         [id-s (make-serializer)]
         [make-id (id-s (lambda ()
                          (set! last-id (+ 1 last-id))
                          last-id))])
  (lambda (balance)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ([s (make-serializer)]
          [id (make-id)])
      (define (dispatch m)
        (cond [(eq? m 'withdraw) withdraw]
              [(eq? m 'deposit) deposit]
              [(eq? m 'balance) balance]
              [(eq? m 'id) id]
              [(eq? m 'serializer) s]
              [else (error "account" "unknown request" m)]))
      dispatch))))

(define (serializer a) (a 'serializer))
(define (id a) (a 'id))

(define (serialized-exchange account1 account2)
  (let* ([s1 (serializer account1)]
         [s2 (serializer account2)]
         [id1 (id account1)]
         [id2 (id account2)]
         [first-s (if (< id1 id2) s1 s2)]
         [second-s (if (< id1 id2) s2 s1)])
    ((first-s (second-s exchange)) account1 account2)))

#| Tests |#

;;; not a great test, nothing is actually in paralell.
(define-test (let ([a (make-account-and-serializer 100)]
                   [b (make-account-and-serializer 50)])
               (serialized-exchange a b)
               (list (a 'balance) (b 'balance)))
             (list 50 100))
