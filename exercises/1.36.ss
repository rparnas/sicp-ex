#|

Exercise 1.36: Modify "fixed-point" so that it prints the
sequence of approximations it generates, using the "newline"
and "display" primitives shown in Exercise 1.22. Then find a
solution to x^x = 1000 by finding a fixed point of x |-->
log(1000) / log(x). (Use Scheme's primitive "log" procedure,
which computes natural logarithms.) Compare the number of
steps this takes with and without average damping. (Note
that you cannot start "fixed-point" with a guess of 1, as
this would cause division by log(1) = 0.)

|#

#| Code from 1.3.3 |#
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

#| Answer |#
(define (fixed-point-trace f first-guess average-damp)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (if average-damp
                    (/ (+ (f guess) guess) 2)
                    (f guess))])
      (if (close-enough? guess next)
    next
    (try next))))
  (set! try (tracize try))
  (try first-guess))

#| Tests 

;; No average damping: 37 guesses
> (define try (tracize try))
> (fixed-point-trace (lambda (x) (/ (log 1000) (log x))) 1.1 #f)
(#<procedure try at 1.36.ss:1085> 1.1)
(#<procedure try at 1.36.ss:1085> 72.47657378429035)
(#<procedure try at 1.36.ss:1085> 1.6127318474109593)
(#<procedure try at 1.36.ss:1085> 14.45350138636525)
(#<procedure try at 1.36.ss:1085> 2.5862669415385087)
(#<procedure try at 1.36.ss:1085> 7.269672273367045)
(#<procedure try at 1.36.ss:1085> 3.4822383620848467)
(#<procedure try at 1.36.ss:1085> 5.536500810236703)
(#<procedure try at 1.36.ss:1085> 4.036406406288111)
(#<procedure try at 1.36.ss:1085> 4.95053682041456)
(#<procedure try at 1.36.ss:1085> 4.318707390180805)
(#<procedure try at 1.36.ss:1085> 4.721778787145103)
(#<procedure try at 1.36.ss:1085> 4.450341068884912)
(#<procedure try at 1.36.ss:1085> 4.626821434106115)
(#<procedure try at 1.36.ss:1085> 4.509360945293209)
(#<procedure try at 1.36.ss:1085> 4.586349500915509)
(#<procedure try at 1.36.ss:1085> 4.535372639594589)
(#<procedure try at 1.36.ss:1085> 4.568901484845316)
(#<procedure try at 1.36.ss:1085> 4.546751100777536)
(#<procedure try at 1.36.ss:1085> 4.561341971741742)
(#<procedure try at 1.36.ss:1085> 4.551712230641226)
(#<procedure try at 1.36.ss:1085> 4.558059671677587)
(#<procedure try at 1.36.ss:1085> 4.55387226495538)
(#<procedure try at 1.36.ss:1085> 4.556633177654167)
(#<procedure try at 1.36.ss:1085> 4.554812144696459)
(#<procedure try at 1.36.ss:1085> 4.556012967736543)
(#<procedure try at 1.36.ss:1085> 4.555220997683307)
(#<procedure try at 1.36.ss:1085> 4.555743265552239)
(#<procedure try at 1.36.ss:1085> 4.555398830243649)
(#<procedure try at 1.36.ss:1085> 4.555625974816275)
(#<procedure try at 1.36.ss:1085> 4.555476175432173)
(#<procedure try at 1.36.ss:1085> 4.555574964557791)
(#<procedure try at 1.36.ss:1085> 4.555509814636753)
(#<procedure try at 1.36.ss:1085> 4.555552779647764)
(#<procedure try at 1.36.ss:1085> 4.555524444961165)
(#<procedure try at 1.36.ss:1085> 4.555543131130589)
(#<procedure try at 1.36.ss:1085> 4.555530807938518)
4.555538934848503

;; Average Damping: 13 guesses
> (fixed-point-trace (lambda (x) (/ (log 1000) (log x))) 1.1 #t)
(#<procedure try at 1.36.ss:1098> 1.1)
(#<procedure try at 1.36.ss:1098> 36.78828689214517)
(#<procedure try at 1.36.ss:1098> 19.352175531882512)
(#<procedure try at 1.36.ss:1098> 10.84183367957568)
(#<procedure try at 1.36.ss:1098> 6.870048352141772)
(#<procedure try at 1.36.ss:1098> 5.227224961967156)
(#<procedure try at 1.36.ss:1098> 4.701960195159289)
(#<procedure try at 1.36.ss:1098> 4.582196773201124)
(#<procedure try at 1.36.ss:1098> 4.560134229703681)
(#<procedure try at 1.36.ss:1098> 4.5563204194309606)
(#<procedure try at 1.36.ss:1098> 4.555669361784037)
(#<procedure try at 1.36.ss:1098> 4.555558462975639)
(#<procedure try at 1.36.ss:1098> 4.55553957996306)
4.555536364911781

|#