#|

Exercise 3.22: Instead of representing a queue as a pair of
pointers, we can build a queue as a procedure with local
state. The local state will consist of pointers to the
beginning and the end of an ordinary list. Thus, the
"make-queue" procedure will have the form

(define (make-queue)
  (let ((front-ptr   ...  )
        (rear-ptr   ...  ))
    <definitions of internal procedures>
    (define (dispatch m)   ... )
    dispatch))

Complete the definition of "make-queue" and provide
implementations of the queue operations using this
representation.

|#

#| Answer |#
(load-ex "3.21") ; for automated tests only

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "front-queue" "called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ([new-pair (cons item '())])
        (cond [(empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch]
              [else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch])))
    (define (delete-queue!)
      (cond [(empty-queue?)
             (error "delete-queue!" "called with an empty queue")]
            [else (set! front-ptr (cdr front-ptr))
                  dispatch]))
    (define (queue->string)
      (string-append "queue: "
                     (format "~a" front-ptr)))
    (define (dispatch m)
      (cond [(eq? m 'empty-queue) empty-queue]
            [(eq? m 'front-queue) front-queue]
            [(eq? m 'insert-queue!) insert-queue!]
            [(eq? m 'delete-queue!) delete-queue!]
            [(eq? m 'queue->string) queue->string]
            [else (error "make-queue" "invalid function" m)]))
    dispatch))

(define (empty-queue? queue) ((queue 'empty-queue)))
(define (front-queue queue) ((queue 'front-queue)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))
(define (queue->string queue) ((queue 'queue->string)))

#| Tests -- regression tests are adequate |#
