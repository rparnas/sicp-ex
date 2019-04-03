#|

Exercise 3.50: Complete the following definition, which
generalizes "stream-map" to allow procedures that take
multiple arguments, analogous to "map" in Section 2.2.1,
Footnote 12.

(define (stream-map proc . argstreams)
  (if (<??> (car argstreams))
      the-empty-stream
      (<??>
       (apply proc (map <??> argstreams))
       (apply stream-map
              (cons proc (map <??> argstreams))))))

|#

#| Code from book |#
(load-ex "1.21") ; prime?

;;; guess at implemention for cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    [(_ car-expression cdr-expression)
     (cons car-expression (delay cdr-expression))]))

;;; guess at implenetation for delay
(define-syntax delay
  (syntax-rules ()
    [(_ expression)
     (memo-proc (lambda () expression))]))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      (void) ;;; changed 'done --> (void)
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond [(stream-null? stream) the-empty-stream]
        [(pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream)))]
        [else (stream-filter pred (stream-cdr stream))]))


;;; guess
(define the-empty-stream '())

;;; guess
(define (stream-null? s)
  (eq? s the-empty-stream))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ([already-run? #f] [result #f])
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

#| Answer |#
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

#| Tests -- infrastructure |#
(define-test (stream-car
               (stream-cdr
                 (stream-filter prime?
                                (stream-enumerate-interval 10000 1000000))))
             10009)

#| Tests |#
(define-test (stream-ref (stream-map + (stream-enumerate-interval 1 100)
                                       (stream-enumerate-interval 1 100)) 49)
             100)
             