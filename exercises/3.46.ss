#|

Exercise 3.46: Suppose that we implement "test-and-set!"
using an ordinary procedure as shown in the text, without
attempting to make the operation atomic. Draw a timing
diagram like the one in Figure 3.29 to demonstrate how the
mutex implementation can fail by allowing two processes to
acquire the mutex at the same time.

|#

#| Code from book |#
(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

#| Answer 

test-and-set! -- assuming (car cell) is false
  a= (get (car cell)) ; action 0/2
  (set-car! cell #t)  ; action 1/3

| cell | (test-and-set! cell)-1 | (test-and-set! cell)-2 |
| (#f) | >[(car cell): #f]      |                        | 0
| (#f) |                        | >[(car cell): #f]      | 2
| (#t) | <[(set-car! cell #t)]  |                        | 1
| (#t) |                        | <[(set-car! cell #t)]  | 3

In this case, both test-and-set! calls believe they have aquired the mutex.

|#

