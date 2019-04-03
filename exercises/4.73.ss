#|

Exercise 4.73: Why does "flatten-stream" use "delay"
explicitly? What would be wrong with defining it as follows:

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flatten-stream (stream-cdr stream)))))

|#

#| Answer 

flatten-stream must use delay because the stream to be flattened might be
infinite. In that case, the non-delayed version cars down the stream forever,
never reaching a base case, when it attempts to evaluate the arguments for
interleave in the outtermost flatten-stream call.

|#