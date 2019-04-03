#|

Exercise 5.36: What order of evaluation does our compiler
produce for operands of a combination? Is it left-to-right,
right-to-left, or some other order? Where in the compiler is
this order determined? Modify the compiler so that it
produces some other order of evaluation. (See the discussion
of order of evaluation for the explicit-control evaluator in
Section 5.4.1.) How does changing the order of operand
evaluation affect the efficiency of the code that constructs
the argument list?

|#

(load-ex "5.33")

#| Answer 

Order is right-to-left, presumably so argument lists can simply be consed up.

To make it left-to-right remove the "reverse" call from construct-arglist and
then do one of the following. I chose to implement (4) below.

(1) Build up the list in reverse order. Afterwards reverse the list at runtime.
This is less eficcient by on reverse operation for every application.

(2) Build up the list in the correct order using the append procedure. Using
append as in the scheme procedure is far less efficient as you need to call it n
times and each time traverses the entire list to find the tail.

(3) Alter the way applications work by changing extend-environment to expect the
already evaluated list of arguments to be in reverse order. This should have no
performance as you should be able to handle this strictly at compile time.

(4) Build up the list in the correct order using an append-like algorithm. To do
this, maintain a reference to both the head and tail of the argument list. For
each element, put that element in a cons cell, set-cdr! the tail of the list to
that new cons cell, and remember that new cons cell as the new tail of the list.

The original algorithm performs a cons for each new element. The new algorithm
performs a list, a set-cdr! and an assign for each new element. An inefficiency
of building up a forwardly linked-list in order using the primitive operations
we are familar with is that the cdr of each new tail cons cell created is wasted
as it is initially set to '() only to be overidden with a reference to the next
tail. We need new cell constructors that leave the values in a cons cell
uninitialized until they are ready to be set.

Consider the implemention details of the original:

(assign argl (op cons) (reg val) (reg argl)) -- make a cons cell
                                             -- put (reg val) in car
                                             -- put (reg argl) in cdr
                                             -- assign argl to cell

A final version of the new approach should be like:
-- make a cons cell
-- put (reg val) in car of the new cell
-- put that cell in the cdr of the cell pointed to by (reg argl)
-- assign argl to that cell

It is difficult to assess the exact performance of the new approach without a
"real" register machine in which we can evaluate performance of primitive lisp
operations (and alternative primitive operations we come up with).

There may be an algorithm for creating a forwardly-linked-list in order that
matches the efficiency of creating that same list backwards, but it may turn
out that creating the list in order is less efficient Theta(n) with respect to
the number of list elements, due to the need to keep a reference to both the
old tail and the new tail as you cons them together.

For what it's worth, running all regression tests takes a trivial amount of
time at either exercise 5.33 and exercise 5.36.

|#

;;; modified from 5.33
(define (construct-arglist operand-codes)
  (let ((operand-codes operand-codes))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ([code-to-get-first-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))])
          (if (null? (cdr operand-codes))
              code-to-get-first-arg
                (preserving '(env)
                 code-to-get-first-arg
                 (preserving '(argl)
                   (code-to-get-rest-args (cdr operand-codes))
                   (make-instruction-sequence '(argl) '() '()))))))))

(define (code-to-get-rest-args operand-codes)
  (let ([code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(argl val) '(argl val)
           '((assign val (op list) (reg val))
             (perform (op set-cdr!) (reg argl) (reg val))
             (assign argl (reg val)))))])
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

#| Tests 

> (reset-ex "5.33")
> (compile-test 
    '(begin
       (define (nothing one two three) (void))
       (nothing (display 'left) (display " ") (display 'right))))
right left
> (define (time-thunk thunk)
   (define start (cpu-time))
   (define result (thunk))
   (define stop (cpu-time))
   (define elapsed (- stop start))
   elapsed)
> (time-thunk (lambda () (run-tests "5.33")))
0

> (reset-ex "5.36")
> (compile-test 
    '(begin
       (define (nothing one two three) (void))
       (nothing (display 'left) (display " ") (display 'right))))
left right
> (define (time-thunk thunk)
   (define start (cpu-time))
   (define result (thunk))
   (define stop (cpu-time))
   (define elapsed (- stop start))
   elapsed)
> (time-thunk (lambda () (run-tests "5.33")))
16
|#
