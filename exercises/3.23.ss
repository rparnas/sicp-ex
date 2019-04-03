#|

Exercise 3.23: A deque ("double-ended queue") is a sequence
in which items can be inserted and deleted at either the
front or the rear. Operations on deques are the constructor
"make-deque", the predicate "empty-deque?", selectors
"front-deque" and "rear-deque", mutators
"front-insert-deque!", "rear-insert-deque!",
"front-delete-deque!", and "rear-delete-deque!". Show how to
represent deques using pairs, and give implementations of
the operations. All operations should be accomplished in
Theta(1) steps.

|#

#| Answer |#

; double-linked list nodes -- (prev-node value next-node)
(define (make-node prev value next)
  (list prev value next))
(define (node-prev node)
  (car node))
(define (node-value node)
  (cadr node))
(define (node-next node)
  (caddr node))
(define (set-node-prev! node new-prev)
  (set-car! node new-prev))
(define (set-node-next! node new-next)
  (set-car! (cddr node) new-next))

; deque -- (front-node rear-node)
(define (make-deque) (list '() '()))
(define (front-node d) (car d))
(define (rear-node d) (cadr d))
(define (set-front-node! d item)
  (set-car! d item))
(define (set-rear-node! d item)
  (set-car! (cdr d) item))

(define (empty-deque? d)
  (null? (front-node d)))

(define (front-deque d)
  (if (empty-deque? d)
      (error "front-deque" "called with an empty deque")
      (node-value (front-node d))))

(define (rear-deque d)
  (if (empty-deque d)
      (error "rear-deque" "called with an empty deque")
      (node-value (rear-node d))))

(define (front-insert-deque! d item)
  (let ([new-front (make-node '() item (front-node d))])
    (if (empty-deque? d)
        (begin
          (set-front-node! d new-front)
          (set-rear-node! d new-front))
        (begin
         (set-node-prev! (front-node d) new-front)
         (set-front-node! d new-front)))))

(define (rear-insert-deque! d item)
  (let ([new-rear (make-node (rear-node d) item '())])
    (if (empty-deque? d)
      (begin
        (set-front-node! d new-rear)
        (set-rear-node! d new-rear))
      (begin
        (set-node-next! (rear-node d) new-rear)
        (set-rear-node! d new-rear)))))

(define (front-delete-deque! d)
  (if (empty-deque? d)
      (error "front-delete-deque!" "called with an empty deque")
      (let ([new-front (node-next (front-node d))])
        (if (null? new-front)
            (begin
              (set-front-node! d new-front)
              (set-rear-node! d new-rear))
            (begin
              (set-node-prev! new-front '())
              (set-front-node! d new-front))))))

(define (rear-delete-deque! d)
  (if (empty-deque? d)
      (error "rear-delete-deque!" "called with an empty deque")
      (let ([new-rear (node-prev (rear-node d))])
        (if (null? new-rear)
            (begin
              (set-front-node! d new-rear)
              (set-rear-node! d new-rear))
            (begin
              (set-node-next! new-rear '())
              (set-rear-node! d new-rear))))))

(define (deque->string d)
  (define (iter result node)
    (if (null? node)
        result
        (iter (string-append result
                             (format "~a" (node-value node))
                             (if (null? (node-next node)) "" " "))
                             (node-next node))))
  (string-append "deque: (" (iter "" (front-node d)) ")"))

#| Tests |#
(define-test (let ([ls '()])
               (define (out) (set! ls (append ls (list (deque->string d)))))
               (define d (make-deque))
               (out)
               (front-insert-deque! d 'a)
               (out)
               (front-insert-deque! d 'b)
               (out)
               (rear-insert-deque! d 'z)
               (out)
               (front-delete-deque! d)
               (out)
               (rear-delete-deque! d)
               (out)
               (rear-delete-deque! d)
               (out)
               ls)
             (list "deque: ()"
                   "deque: (a)"
                   "deque: (b a)"
                   "deque: (b a z)"
                   "deque: (a z)"
                   "deque: (a)"
                   "deque: ()"
                   ))




