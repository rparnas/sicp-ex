#|

Exercise 3.38: Suppose that Peter, Paul, and Mary share a
joint bank account that initially contains $100.
Concurrently, Peter deposits $10, Paul withdraws $20, and
Mary withdraws half the money in the account, by executing
the following commands:

Peter: (set! balance (+ balance 10))
Paul:  (set! balance (- balance 20))
Mary:  (set! balance (- balance (/ balance 2)))

a. List all the different possible values for "balance"
after these three transactions have been completed, assuming
that the banking system forces the three processes to run
sequentially in some order.

b. What are some other values that could be produced if the
system allows the processes to be interleaved? Draw timing
diagrams like the one in Figure 3.29 to explain how these
values can occur.

|#

#| Answer 

a. 100 +10 -20 /2 => 45
   100 +10 /2 -20 => 35
   100 -20 +10 /2 => 45
   100 -20 /2 +10 => 50
   100 /2 +10 -20 => 40
   100 /2 -20 +10 => 40
   ====================
   (35 40 45 50)

b. Peter: a= balance
          b= a + 10 (doesn't matter when this happens)
          balance= b
          =========
          a=balance          : action 0
          balance=(a+10)     : action 1

   Paul:  a=balance          : action 2
          balance=(a-20)     : action 3
    
   Mary:  a=balance
          b=(a/2) (doesn't matter when this happens)
          c=balance
          d=(c-b) (doesn't matter when this happens)
          balance=d
          =========
          a=balance          : action 4
          c=balance          : action 5
          balance= (c-(a/2)) : action 6

  There are 7 meaningful actions which has 5040 permutations.
  0 is before 1, 2 is before 3, 4 is before 5, 5 is before 6.
  This leaves 210 permutations.

  Running these permutations, the unique possible values are
  (25 30 35 40 45 50 55 60 70 80 90 110)

  The following actions in order result in 25: (0 2 1 4 3 5 6)
  | Bank | Peter             | Paul              | Mary                    |
  | 100  |                   |                   |                         |
  | 100  |>[get: 100]        |                   |                         | 0
  | 100  |     |             |>[get: 100]        |                         | 2
  | 110  |<[set: (+ 100 10)] |     |             |                         | 1
  | 110  |                   |     |             |>[get: 110]              | 4
  |  80  |                   |<[set: (- 100 20)] |     |                   | 3
  |  80  |                   |                   |     |      >[get: 80]   | 5
  |  80  |                   |                   |     |           |       |
  |  25  |                   |                   |<[set: (- 80 (/ 110 2))] | 6

|#

#| Tests -- infrastructure |#
(load-ex "2.41") ; permutations

;;; returns true if a is found before b in the given set.
(define (a-is-before-b a b set)
  (cond [(= a (car set)) #t]
        [(= b (car set)) #f]
        [else (a-is-before-b a b (cdr set))]))

;;; executes the given runner with the given order
(define (execute-runner r order)
  (if (null? order)
      (r 'return)
      (begin
        (r (car order))
        (execute-runner r (cdr order)))))

;;; return possible results given executing run with the given integers
(define (possibilities make-runner action-count constraints)
  (define (sorter a b)
    (if (and (number? a) (number? b))
        (< a b)
        (sorter (car a) (car b))))
  (let* ([perm (permutations (iota action-count))]
         [orders (filter (lambda (set) (andmap (lambda (c)
                                                 (a-is-before-b (car c) (cadr c) set))
                                               constraints))
                         perm)])
  (sort sorter (map (lambda (order) (cons (execute-runner (make-runner) order) order))
                    orders))))

(define (possible-results possibilities)
  (define (unique ls)
    (define (iter result last ls)
      (cond [(null? ls) result]
            [(equal? (car ls) last) (iter result last (cdr ls))]
            [else (iter (cons (car ls) result) (car ls) (cdr ls))]))
    (reverse (iter '() #f ls)))
  (unique (map car possibilities)))

#| Tests |#
(define (make-run_338)
    (let ([balance 100]
          [peter_a #f]
          [paul_a #f]
          [mary_a #f]
          [mary_c #f])
      (lambda (step)
        (cond [(equal? step 'return) balance]
              [(= step 0) (set! peter_a balance)]
              [(= step 1) (set! balance (+ peter_a 10))]
              [(= step 2) (set! paul_a balance)]
              [(= step 3) (set! balance (- paul_a 20))]
              [(= step 4) (set! mary_a balance)]
              [(= step 5) (set! mary_c balance)]
              [(= step 6) (set! balance (- mary_c (/ mary_a 2)))]))))

(define-test (length (permutations '(0 1 2 3 4 5 6)))
             5040)

(define-test (possible-results (possibilities make-run_338 7 '((0 1) (2 3) (4 5) (5 6))))
             '(25 30 35 40 45 50 55 60 70 80 90 110))