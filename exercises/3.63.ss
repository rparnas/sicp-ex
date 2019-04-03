#|

Exercise 3.63: Louis Reasoner asks why the "sqrt-stream"
procedure was not written in the following more
straightforward way, without the local variable "guesses":

(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream x))))

Alyssa P. Hacker replies that this version of the procedure
is considerably less efficient because it performs redundant
computation. Explain Alyssa's answer. Would the two versions
still differ in efficiency if our implementation of "delay"
used only "(lambda () <exp>)" without using the optimization
provided by "memo-proc" (Section 3.5.1)?

|#

(load-ex "3.62")

#| Code from book |#

(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve guess x) (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
      (stream-map (lambda (guess)
                    (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (sqrt-stream-bad x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream-bad x))))

#| Answer 

;;; map for reference
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

In the old version each new index of the stream is created by stream-map which
conses applying a procedure to the first element of the previous nested stream
to additional maps. This works because each new stream created has a reference
to the previous stream (the stream passed to stream-map).

 created by 1st call to sqrt-stream
 v
[s0: g0...
[s0: g0 [m0->s0: g1...
[s0: g0 [m0->s0: g1 [m1->m0: g2...
         ^           ^
         created by 1st and 2nd call to stream-map

In the new version, after acessing index 1, s0 was created by the outermost call
to sqrt-stream, s1 was created by this same call for passing into stream-map and
m0 was created by stream-map.

[s0: g0a [m0->s1: g1a...
[s1: g0b...

The problem is m0 references s1 a fresh stream with no memoized results. When
you access index 2 to calculate m0's cdr, it needs s1's cdr. s1's cdr
redundantly stream-maps yet another new stream redundantly recalculating guess 1
before allowing the recursively mapped stream to calculate guess 2 for the first
time.

[s0: g0a [m0->s1: g1a [m2->m1: g2a...
[s1: g0b [m1->s2: g1b...
[s2: g0c...

Each new index creates one new stream at the bottom and then calculate the tail
value of all streams that exist. (In addition to the redudant calculation ithas
to traverse down n streams to get to where that new stream should go).

[s0: g0a [m0->s1: g1a [m2->m1: g2a [m5->m4 g3a
[s1: g0b [m1->s2: g1b [m4->m3: g2b...
[s2: g0c [m3->s3: g1c...
[s3: g0d...

The number of calls to sqrt-improve (and the number of cells in memory to keep
track of the results is (n) in the old [n(n+1)/2] in the new version.

If memoization is turned off consider the signature of (stream-map proc s). Even
though the stream created by stream-map has a reference to s, that s never
mutates to memoize previous results. As far as map is concerned you might as
well have passed in a brand new stream.

Thus without memoization, old and new behave the same: [n(n+1)/2] improve calls
to access index n.

|#

#| Tests 

;;;; memoized
> (define c 0)
> (define (sqrt-improve guess x)
    (set! c (+ c 1))
    (average guess (/ x guess)))

> (begin (set! c 0) (stream-ref (sqrt-stream 4) 5) c)
5
> (begin (set! c 0) (stream-ref (sqrt-stream 4) 50) c)
50

> (begin (set! c 0) (stream-ref (sqrt-stream-bad 4) 5) c)
15
> (begin (set! c 0) (stream-ref (sqrt-stream-bad 4) 50) c)
1275

;;;; not memoized
> (define (memo-proc proc) proc)

> (begin (set! c 0) (stream-ref (sqrt-stream 4) 6) c)
21
>  (begin (set! c 0) (stream-ref (sqrt-stream 4) 50) c)
1275

> (begin (set! c 0) (stream-ref (sqrt-stream-bad 4) 6) c)
21
> (begin (set! c 0) (stream-ref (sqrt-stream-bad 4) 50) c)
1275

|#
