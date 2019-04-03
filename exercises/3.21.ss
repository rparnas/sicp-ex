#|

Exercise 3.21: Ben Bitdiddle decides to test the queue
implementation described above. He types in the procedures
to the Lisp interpreter and proceeds to try them out:

(define q1 (make-queue))
(insert-queue! q1 'a)
 ((a) a) 
(insert-queue! q1 'b)
 ((a b) b) 
(delete-queue! q1)
 ((b) b) 
(delete-queue! q1)
 (() b) 

"It's all wrong!" he complains. "The interpreter's response
shows that the last item is inserted into the queue twice.
And when I delete both items, the second "b" is still there,
so the queue isn't empty, even though it's supposed to be."
Eva Lu Ator suggests that Ben has misunderstood what is
happening. "It's not that the items are going into the queue
twice," she explains. "It's just that the standard Lisp
printer doesn't know how to make sense of the queue
representation. If you want to see the queue printed
correctly, you'll have to define your own print procedure
for queues." Explain what Eva Lu is talking about. In
particular, show why Ben's examples produce the printed
results that they do. Define a procedure "print-queue" that
takes a queue as input and prints the sequence of items in
the queue.

|#

#| Code from book |#
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "front-queue" "called with an empty queue")
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "delete-queue!" "called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

#| Answer 

The queue is a cons cell with the car being a list of all items in the queue and
the cdr being a pointer to the tail item in that same list. So every time you
are going to see both the entire list and the tail item in the list.

A special case is after all items in the queue are deleted. There is still a
pointer to the former final item in the list but this is moot. It exists because
the delete operation didn't bother to clear it out because it causes no problems
according to the queue api.

|#

(define (queue->string queue)
  (string-append "queue: "
                 (format "~a" (front-ptr queue))))

(define (display-queue queue)
  (display (queue->string queue))
  (newline))

(define-test (let ([ls '()])
               (define (out) (set! ls (append ls (list (queue->string q1)))))
               (define q1 (make-queue))
               (out)
               (insert-queue! q1 'a)
               (out)
               (insert-queue! q1 'b)
               (out)
               (delete-queue! q1)
               (out)
               (delete-queue! q1)
               (out)
               ls)
             (list "queue: ()"
                   "queue: (a)"
                   "queue: (a b)"
                   "queue: (b)"
                   "queue: ()"))
