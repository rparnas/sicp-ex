#|

Exercise 3.47: A semaphore (of size n) is a generalization
of a mutex. Like a mutex, a semaphore supports acquire and
release operations, but it is more general in that up to n
processes can acquire it concurrently. Additional processes
that attempt to acquire the semaphore must wait for release
operations. Give implementations of semaphores

a. in terms of mutexes

b. in terms of atomic "test-and-set!" operations.

|#

#| Answer |#

;;; a. protect count with a mutex.
(define (make-semaphore n)
  (let ([count n]
        [mutex (make-mutex)])
  (define (the-semaphore m)
    (cond [(eq? m 'acquire)
           (mutex 'aquire)
           (cond [(= count 0)
                  (mutex 'release)
                  (the-semaphore 'aquire)] ; retry
                 [else
                  (set! count (- count 1))
                  (mutex 'release)])]
          [(eq? m 'release)
           (mutex 'aquire)
           (set! count (+ count 1))
           (mutex 'release)]))
  the-semaphore))

;;; b. use test-and-set!, clear! directly instead of as a mutex.
(define (make-semaphore n)
  (let ([count 0]
        [cell (list #f)])
  (define (the-semaphore m)
    (cond [(eq? m 'acquire)
           (if (test-and-set! cell)
               (the-semaphore 'aquire)
               (cond [(= count 0)
                      (clear! cell)
                      (the-semaphore 'aquire)]
                     [else
                      (set! count (+ count 1))
                      (clear! cell)]))]
           [(eq? m 'release)
            (if (test-and-set! cell)
                (the-semaphore 'release)
                (begin
                  (set! count (+ count 1))
                  (clear! cell)))]))
  the-semaphore))

#| Notes

It would better to change the cell to store an integer count and just redefine
make-mutex in terms of semaphores.

|#