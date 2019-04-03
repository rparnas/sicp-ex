#|

Exercise 3.66: Examine the stream "(pairs integers
integers)". Can you make any general comments about the
order in which the pairs are placed into the stream? For
example, approximately how many pairs precede the pair (1,
100)? the pair (99, 100)? the pair (100, 100)? (If you can
make precise mathematical statements here, all the better.
But feel free to give more qualitative answers if you find
yourself getting bogged down.)

|#

(load-ex "3.65")

#| Code from book |#
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

#| Answer 

(1 100) is at i = 197
(99 100) is at i = 950737950171172051122527404030
(100 100 is at i = 633825300114114700748351602686

|#

#| Notes

> (define (get-pair i) (stream-ref (pairs integers integers) i))
> (map get-pair (iota 20))
 ((1 1)   ; 0
  (1 2)   ; 1
  (2 2)   ; 2
  (1 3)   ; 3
  (2 3)   ; 4
  (1 4)   ; 5
  (3 3)   ; 6 
  (1 5)   ; 7
  (2 4)   ; 8
  (1 6)   ; 9
  (3 4)   ; 10
  (1 7)   ; 11
  (2 5)   ; 12
  (1 8)   ; 13
  (4 4)   ; 14
  (1 9)   ; 15
  (2 6)   ; 16
  (1 10)  ; 17
  (3 5)   ; 18
  (1 11)) ; 19

(1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (1 10) (1 11)...
      (2 2) (2 3) (2 4) (2 5) (2 6)...
            (3 3) (3 4) (3 5)...
                  (4 4)...

(define (f n m) (- (expt 2 n) 2))

~indicies~
( 0 ) ( 1 ) ( 3 ) ( 5 ) ( 7 ) ( 9 ) ( 11) ( 13) ( 15) ( 17) ( 19)...
      ( 2 ) ( 4 ) ( 8 ) ( 12) ( 16)...
            ( 6 ) ( 10) ( 18)...
                  ( 14)...

~cost to move right~
( 1 ) ( 2 ) ( 2 ) ( 2 ) ( 2 ) ( 2 ) ( 2 ) ( 2 ) ( 2 ) ( x )...
      ( 2 ) ( 4 ) ( 4 ) ( 4 ) ( x )...
            ( 4 ) ( 8 ) ( x )...
                  ( x )

~cost to down-right diagonally~
( 2 ) ( x ) ( x ) ( x ) ( x ) ( x ) ( x ) ( x ) ( x ) ( x )...
      ( 4 ) ( x ) ( x ) ( x ) ( x )...
            ( 8 ) ( x ) ( x )...
                  ( ? )...

; Guess that the cost to move from (4 4) to (5 5) is 16 thus (5 5) is at 14+16=30.
; Guess that the cost to move from (5 5) to (6 6) is 2^5=32 thus (5 5) is at 30+32=62
> (stream-ref (pairs integers integers) 30)
(5 5)
> (stream-ref (pairs integers integers) 62)
(6 6)

(5 5) (4th row) 2^1 + 2^2 + 2^3 + 2^4 = 30
(6 6) (5th row) 2^1 + 2^2 + 2^3 + 2^4 + 2^5 = 62
(n n) (nth row) sum of powers of 2, 1 to n = 2^(n+1) - 1 - 2^0
thus the following would compute diagonals:

(define (f n m) (- (expt 2 n) 2))

Guess that the cost to move from i=18:(3 5) to (3 6) is 8. True, i=26.
Guess that the cost to move from i=14:(4 4) to (4 5) is 8. True, i=22.
Guess that the cost to move from (4 5) to (4 6) is 16. True, i=38.
Guess that the cost to move from (4 6) to (4 7) is 16. True, i=54.
> (stream-ref (pairs integers integers) 26)
(3 6)
> (stream-ref (pairs integers integers) 22)
(4 5)
> (stream-ref (pairs integers integers) 38)
(4 6)
> (stream-ref (pairs integers integers) 54)
(4 7)

The cost to move to the second thing in the row is 2^n
The cost to move right additional spaces is 2^n+1

column 0: 0
column 1: 2^n
column 2: 2^n + 2^(n+1)
column 3: 2^n + [2 * 2^(n + 1)]
column 4: 2^n + [3 * 2^(n + 1)]

column 0: 2^n + [m * 2^(n + 1)] = -1
column 1: 2^n + [m * 2^(n + 1)] = 1
column 2: 2^n + [m * 2^(n + 1)] = 3
column 3: 2^n + [m * 2^(n + 1)] = 5
column 4: 2^n + [m * 2^(n + 1)] = 7

~pairs~
(1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (1 10) (1 11)...
      (2 2) (2 3) (2 4) (2 5) (2 6)...
            (3 3) (3 4) (3 5)...
                  (4 4)...

~indicies~
( 0 ) ( 1 ) ( 3 ) ( 5 ) ( 7 ) ( 9 ) ( 11) ( 13) ( 15) ( 17) ( 19)...
      ( 2 ) ( 4 ) ( 8 ) ( 12) ( 16)...
            ( 6 ) ( 10) ( 18)...
                  ( 14)...

|#

#| Tests |#

(define (f i j)
  (if (> i j)
      (error "i must be <= j" i j)
        (cond [(and (= i j))
               (- (expt 2 i) 2)]
              [else 
               (+ (- (* i (expt 2 i)))
                  (* j (expt 2 i))
                  (expt 2 (- i 1))
                  (- 2))])))

(define-test (let* ([x (iota 1000)]
                    [y (map (lambda (i) (stream-ref (pairs integers integers) i)) x)]
                    [z (map (lambda (p) (f (car p) (cadr p))) y)])
               (andmap (lambda (a b) (= a b)) x z))
             #t)

(define-test (f 1 100) 197)
(define-test (f 99 100) 950737950171172051122527404030)
(define-test (f 100 100) 1267650600228229401496703205374)
