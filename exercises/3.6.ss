#|

Exercise 3.6: It is useful to be able to reset a
random-number generator to produce a sequence starting from
a given value. Design a new "rand" procedure that is called
with an argument that is either the symbol "generate" or the
symbol "reset" and behaves as follows: "(rand 'generate)"
produces a new random number; "((rand 'reset)"<new-value>")"
resets the internal state variable to the designated
<new-value>. Thus, by resetting the state, one can generate
repeatable sequences. These are very handy to have when
testing and debugging programs that use random numbers.

|#

(define rand
  (let ([seed 7])
    (define (rand-update x)
      (let ([a 109]
            [b 17]
            [m 19])
        (mod (+ (* a x) b) m)))
    (lambda (arg)
      (cond [(eq? arg 'generate) 
             (begin
              (set! seed (rand-update seed))
              seed)]
            [(eq? arg 'reset)
             (lambda (new-value)
               (set! seed new-value)
               "reset")]
            [else 
             (error "rand" "no such method" arg)]))))

#| Tests 

; I can't figure out how to automatically test this.

> ((rand 'reset) 7)
"reset"
> (rand 'generate)
1
> (rand 'generate)
12
> (rand 'generate)
14
> (rand 'generate)
4
> (rand 'generate)
16
> ((rand 'reset) 7)
"reset"
> (rand 'generate)
1
> (rand 'generate)
12
> (rand 'generate)
14
> (rand 'generate)
4
> (rand 'generate)
16
>

|#

#| Notes

The book did not adequately explain how to make a PRNG.

|#