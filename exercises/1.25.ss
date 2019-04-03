#|

Exercise 1.25: Alyssa P. Hacker complains that we went to a
lot of extra work in writing "expmod". After all, she says,
since we already know how to compute exponentials, we could
have simply written

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

Is she correct? Would this procedure serve as well for our
fast prime tester? Explain.

|#

#| Answer

Yes, but this version doesn't keep the size of numbers down
which could be harsh for the underlying math implementation.

|#

#| Test -- manual |#

(load-ex "1.24")

#|

; Before
> (define expmod (tracize expmod))
> (expmod 50 60 109123)
(#<procedure expmod at 1.24.ss:452> 50 60 109123)
(#<procedure expmod at 1.24.ss:452> 50 30 109123)
(#<procedure expmod at 1.24.ss:452> 50 15 109123)
(#<procedure expmod at 1.24.ss:452> 50 14 109123)
(#<procedure expmod at 1.24.ss:452> 50 7 109123)
(#<procedure expmod at 1.24.ss:452> 50 6 109123)
(#<procedure expmod at 1.24.ss:452> 50 3 109123)
(#<procedure expmod at 1.24.ss:452> 50 2 109123)
(#<procedure expmod at 1.24.ss:452> 50 1 109123)
(#<procedure expmod at 1.24.ss:452> 50 0 109123)
47482

; After
> (define (expmod base exp m)
    (r (expt base exp) m))
> (define expmod (tracize expmod))
> (define r (tracize remainder))
> (expmod 50 60 109123)
(#<procedure expmod> 50 60 109123)
(#<procedure remainder> 867361737988403547205962240695953369140625000000000000000000000000000000000000000000000000000000000000 109123)
47482

|#