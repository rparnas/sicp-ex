#|

Exercise 3.43: Suppose that the balances in three accounts
start out as $10, $20, and $30, and that multiple processes
run, exchanging the balances in the accounts. Argue that if
the processes are run sequentially, after any number of
concurrent exchanges, the account balances should be $10,
$20, and $30 in some order. Draw a timing diagram like the
one in Figure 3.29 to show how this condition can be
violated if the exchanges are implemented using the first
version of the account-exchange program in this section. On
the other hand, argue that even with this "exchange"
program, the sum of the balances in the accounts will be
preserved. Draw a timing diagram to show how even this
condition would be violated if we did not serialize the
transactions on individual accounts.

|#

#| Answer 

;;; first version of the account-exchange program
(define (exchange account 1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    (account1 'withdraw difference)
    (account2 'deposit difference)))

Swap(A, B)
  a=(A 'balance)        ; action 0 / 4 / 8
  b=(B 'balance)        ; action 1 / 5 / 9
  (A 'withdraw (- a b)) ; action 2 / 6 / 10
  (B 'deposit (- a b))  ; action 3 / 7 / 11

A simple example is two Swap(A, B) happening at the same time. The possibilities
for (A B C) are: ((10 20 30) (20 10 30) (30 0 30)). For example, (20 10 30) can
be achieved with actions in the order (0 1 2 4 5 3 6 7):

| A  | B  | C  |  (swap A B)-1             |  (swap A B)-2             |
| 10 | 20 | 30 | >[get A: 10]              |                           | 0
| 10 | 20 | 30 |   |    |     >[get B: 20] |                           | 1
| 10 | 20 | 30 |   |    |       |      |   |                           |
| 20 | 20 | 30 |   | <[w A: (- 10 20)] |   |                           | 2
| 20 | 20 | 30 |   |                   |   | >[get A: 20]              | 4
| 20 | 20 | 30 |   |                   |   |   |    |     >[get B: 20] | 5
| 20 | 10 | 30 | <[d B: (- 10 20)       ]  |   |    |       |      |   | 3
| 20 | 10 | 30 |                           |   | <[w A: (- 20 20)] |   | 6
| 20 | 10 | 30 |                           | <[d B: (- 20 20)       ]  | 7

All possibilities sum up to 60 but the second two possibilities are incorrect in
our example are incorrect. The sum of the accounts can never change because each
eventually swap withdraws and deposits an identical amount.

NOTE: The book is a bit vague, but a better analysis might be that the sum of
all accounts is always 60, or the program will crash due an exception thrown by
withdraw. Consider three simultaneous (swap C A). All swaps could in paralell
compute the difference to be (- C A) = (- 30 10) = 20. Then all three swaps
could withdraw 20 from C, causing an exception. This corresponds to actions (0 1
2 4 5 8 9 2 6).

| A  | B  | C  |  (swap C A)-1             | (swap C A)-2              | (swap C A)-4              |
| 10 | 20 | 30 | >[get C: 30]              |                           |                           | 0
| 10 | 20 | 30 |        |     >[get A: 10] |                           |                           | 1
| 10 | 20 | 30 |        |            |     | >[get C: 30]              |                           | 4                            
| 10 | 20 | 30 |        |            |     |        |     >[get A: 10] |                           | 5
| 10 | 20 | 30 |        |            |     |        |            |     | >[get C: 30]              | 8                           
| 10 | 20 | 30 |        |            |     |        |            |     |              >[get A: 10] | 9
| 10 | 20 | 10 | <[wd C: (- 30 10)    ]    |        |            |     |                           | 2
| 10 | 20 | ?? |                           | <[wd C: (- 30 10)    ]    |                           | 6

;;; Show how sums up to 60 will be violated if additionally we allow withdraws
and deposits to happen in paralell.

The actions of a swap, including our analysis of the actions of withdraw
and deposit from 3.41:

Swap(A, B)
  a=(A 'balance)           ; action 0/7
  b=(B 'balance)           ; action 1/8
  w_a= A                   ; action 2/9  -- withdraw A -- for (if (>= balance... -- if false crash
  w_b= A                   ; action 3/10
  (set! A (- w_b (- a b))) ; action 4/11
  d_a= B                   ; action 5/12 -- deposit B
  (set! B (+ d_a (- a b))) ; action 6/13

There are too many permutations to run through them all. However, the
permutation (0 7 8 1 2 9 3 4 10 11 5 12 6 13) results in (A B C) of (30 10 30)
which sums to 70.

> (execute-runner (make-run_343-3) '(0 7 1 8 2 9 3 4 10 11 5 12 6 13))
(30 10 30)

|         | First swap                                                              | Second swap                                                             |
| A  | B  | (swap A B)                | (withdraw A -10)     | (deposit B -10)      | (swap A B)                | (withdraw A -10)     | (deposit B -10)      |
| 10 | 20 | >[get A: 10]              |                      |                      |                           |                      |                      | 0
| 10 | 20 |   |    |                  |                      |                      | [get A: 10]>              |                      |                      | 7
| 10 | 20 |   |    |     >[get A: 20] |                      |                      |  |    |                   |                      |                      | 1
| 10 | 20 |   |    |            |  |  |                      |                      |  |    |      >[get B: 20] |                      |                      | 8
| 10 | 20 |   | [wd A: (- 20 10) ] |  | >[get A: 10]         |                      |  |    |            |  |   |                      |                      | 2
| 10 | 20 |   |                    |  |                      |                      |  | [wd A: (- 20 10)]  |   | >[get A: 10]         |                      | 9
| 10 | 20 |   |                    |  | >[get A: 10]         |                      |  |                    |   |                      |                      | 3
| 10 | 20 |   |                    |  |        |             |                      |  |                    |   |                      |                      |
| 20 | 20 |   |                    |  | <[set A: (- 10 -10)] |                      |  |                    |   |                      |                      | 4
| 20 | 20 |   |                    |  |                      |                      |  |                    |   | >[get A: 20]         |                      | 10
| 20 | 20 |   |                    |  |                      |                      |  |                    |   |        |             |                      |
| 30 | 20 |   |                    |  |                      |                      |  |                    |   | <[set A: (- 20 -10)] |                      | 11
| 30 | 20 | [d B: (- 20 10)         ] |                      | >[get B: 20]         |  |                    |   |                      |                      | 5
| 30 | 20 |                           |                      |        |             | [d B: (- 20 10)         ] |                      | >[get B: 20]         | 12
| 30 | 10 |                           |                      | <[set B: (+ 20 -10)] |                           |                      |        |             | 6
| 30 | 10 |                           |                      |                      |                           |                      | <[set B: (+ 20 -10)] | 13

|#

#| Tests |#
(load-ex "3.40")

;; serialized account access and parallel swaps for two Swap(A,B)
(define (make-run_343-1)
  (let ([A 10] [B 20] [C 30] [a0 #f] [b0 #f] [a1 #f] [b1 #f] [withdraw-error #f])
    (lambda (step)
            (cond [(equal? step 'return) (if withdraw-error '(-1 -1 -1) (list A B C))]
                  ; Swap(A, B)
                  [(= step 0) (set! a0 A)]
                  [(= step 1) (set! b0 B)]
                  [(= step 2) (set! A (- A (- a0 b0))) ; withdraw
                              (if (< A 0)
                                  (set! withdraw-error #t)
                                  (void))]
                  [(= step 3) (set! B (+ B (- a0 b0)))] ; deposit
                  ; Swap(A, B) -- another parallel invocation
                  [(= step 4) (set! a1 A)]
                  [(= step 5) (set! b1 B)]
                  [(= step 6) (set! A (- A (- a1 b1))) ; withdraw
                              (if (< A 0)
                                  (set! withdraw-error #t)
                                  (void))]
                  [(= step 7) (set! B (+ B (- a1 b1)))])))) ; deposit

;; paralell account access and parallel swaps for two Swap(A,B)
(define (make-run_343-3)
  (let ([A 10] [B 20] [C 30] [withdraw-error #f]
        [a0 #f] [b0 #f] [wa0 #F] [wb0 #f] [da0 #f] [db0 #f]
        [a1 #f] [b1 #f] [wa1 #F] [wb1 #f] [da1 #f] [db1 #f])
    (lambda (step)
      (cond [(equal? step 'return) (if withdraw-error '(-1 -1 -1) (list A B C))]
            ; Swap(A, B)
            [(= step 0) (set! a0 A)]
            [(= step 1) (set! b0 B)]
            [(= step 2) (set! wa0 B)
                        (if (< wa0 (- a0 b0))
                            (set! withdraw-error #t)
                            (void))] ; withdraw
            [(= step 3) (set! wb0 A)]
            [(= step 4) (set! A (- wb0 (- a0 b0)))]
            [(= step 5) (set! da0 B)] ; deposit
            [(= step 6) (set! B (+ da0 (- a0 b0)))]
            ; Swap(A, B)
            [(= step 7) (set! a1 A)]
            [(= step 8) (set! b1 B)]
            [(= step 9) (set! wa1 A) ; withdraw
                        (if (< wa1 (- a1 b1))
                            (set! withdraw-error #t)
                            (void))]
            [(= step 10) (set! wb1 A)]
            [(= step 11) (set! A (- wb1 (- a1 b1)))]
            [(= step 12) (set! da1 B)] ; depoist
            [(= step 13) (set! B (+ da1 (- a1 b1)))]))))

(define-test (possible-results (possibilities make-run_343-1 8 '((0 1) (1 2) (2 3)
                                                                (4 5) (5 6) (6 7))))
             '((10 20 30) (20 10 30) (30 0 30)))

; takes too long
; (define-test (possible-results (possibilities make-run_343-2 14 '((0 1) (1 2) (3 4) (4 5) (5 6)
;                                                                  (7 8) (8 9) (9 10) (10 11) (11 12) (12 13))))
;             '())
