#|

Exercise 5.5: Hand-simulate the factorial and Fibonacci
machines, using some nontrivial input (requiring execution
of at least one recursive call). Show the contents of the
stack at each significant point in the execution.

|#

#| Code from book -- Figure 5.11 -- a recursive factorial machine

01: (controller
02:   (assign continue (label fact-done))     ; set up final return address
03: fact-loop
04:   (test (op =) (reg n) (const 1))
05:   (branch (label base-case))
06:   (save continue)
07:   (save n)
08:   (assign n (op -) (reg n) (const 1))
09:   (assign continue (label after-fact))
10:   (goto (label fact-loop))
11: after-fact
12:   (restore n)
13:   (restore continue)
14:   (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
15:   (goto (reg continue))                   ; return to caller
16: base-case
17:   (assign val (const 1))                  ; base case: 1! = 1
18:   (goto (reg continue))                   ; return to caller
19: fact-done)

|#

#| Answer -- (factorial 3)

lines  | (reg continue)     | (reg n) | (reg val) | stack
       | ?                  | 3       | ?         | '()
 02    | (label fact-done)  | 3       | ?         | '()
 06-07 | (label fact-done)  | 3       | ?         | '(3 (label fact-done))
 08-09 | (label after-fact) | 2       | ?         | '(3 (label fact-done))
 06-07 | (label after-fact) | 2       | ?         | '(2 (label after-fact) 3 (label fact-done))
 08-09 | (label after-fact) | 1       | ?         | '(2 (label after-fact) 3 (label fact-done))
 17    | (label after-fact) | 1       | 1         | '(2 (label after-fact) 3 (label fact-done))
 12-13 | (label after-fact) | 2       | 1         | '(3 (label fact-done))
 14    | (label after-fact) | 2       | 2         | '(3 (label fact-done))
 12-13 | (label fact-done)  | 3       | 2         | '()
 14    | (label fact-done)  | 3       | 6         | '()

|#

#| Code from book -- Figure 5.12 -- Controller for a machine to compute Fibonacci numbers.

01: (controller
02:   (assign continue (label fib-done))
03: fib-loop
04:   (test (op <) (reg n) (const 2))
05:   (branch (label immediate-answer))
06:   ;; set up to compute Fib(n - 1)
07:   (save continue)
08:   (assign continue (label afterfib-n-1))
09:   (save n)                              ; save old value of n
10:   (assign n (op -) (reg n) (const 1))   ; clobber n to n - 1
11:   (goto (label fib-loop))               ; perform recursive call
12: afterfib-n-1                            ; upon return, val contains Fib(n - 1)
13:   (restore n)
14:   (restore continue)
15:   ;; set up to compute Fib(n - 2)
16:   (assign n (op -) (reg n) (const 2))
17:   (save continue)
18:   (assign continue (label afterfib-n-2))
19:   (save val)                            ; save Fib(n - 1)
20:   (goto (label fib-loop))
21: afterfib-n-2                            ; upon return, val contains Fib(n - 2)
22:   (assign n (reg val))                  ; n now contains Fib(n - 2)
23:   (restore val)                         ; val now contains Fib(n - 1)
24:   (restore continue)
25:   (assign val (op +) (reg val) (reg n)) ;  Fib(n - 1) +  Fib(n - 2)
26:   (goto (reg continue))                 ; return to caller, answer is in val
27: immediate-answer
28:   (assign val (reg n))                  ; base case:  Fib(n) = n
29:   (goto (reg continue))
30: fib-done)

|#

#| Answer -- (fib 5) 

lines  | (reg continue)        | (reg n) | (reg val) | stack
 02    | (label fib-done)      | 5       | ?         | '()
 07-08 | (label after-fib-n-1) | 5       | ?         | '((label fib-done))
 09-10 | (label after-fib-n-1) | 4       | ?         | '(5 (label fib-done))
 07-08 | (label after-fib-n-1) | 4       | ?         | '((label after-fib-n-1) 5 (label fib-done))
 09-10 | (label after-fib-n-1) | 3       | ?         | '(4 (label after-fib-n-1) 5 (label fib-done))
 07-08 | (label after-fib-n-1) | 3       | ?         | '((label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 09-10 | (label after-fib-n-1) | 2       | ?         | '(3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 07-08 | (label after-fib-n-1) | 2       | ?         | '((label after-fib-n-1) 3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 09-10 | (label after-fib-n-1) | 1       | ?         | '(2 (label after-fib-n-1) 3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 28    | (label after-fib-n-1) | 1       | 1         | '(2 (label after-fib-n-1) 3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 13-14 | (label after-fib-n-1) | 2       | 1         | '(3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 16    | (label after-fib-n-1) | 0       | 1         | '(3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 17-19 | (label after-fib-n-2) | 0       | 1         | '(1 (label after-fib-n-1) 3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 28    | (label after-fib-n-2) | 0       | 0         | '(1 (label after-fib-n-1) 3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 22    | (label after-fib-n-2) | 0       | 0         | '(1 (label after-fib-n-1) 3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 23-24 | (label after-fib-n-1) | 0       | 1         | '(3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
 25    | (label after-fib-n-1) | 0       | 1         | '(3 (label after-fib-n-1) 4 (label after-fib-n-1) 5 (label fib-done))
. . . and so on.

|#
