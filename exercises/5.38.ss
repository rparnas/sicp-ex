#|

Exercise 5.38: Our compiler is clever about avoiding
unnecessary stack operations, but it is not clever at all
when it comes to compiling calls to the primitive procedures
of the language in terms of the primitive operations
supplied by the machine. For example, consider how much code
is compiled to compute "(+ a 1)": The code sets up an
argument list in "argl", puts the primitive addition
procedure (which it finds by looking up the symbol "+" in
the environment) into "proc", and tests whether the
procedure is primitive or compound. The compiler always
generates code to perform the test, as well as code for
primitive and compound branches (only one of which will be
executed). We have not shown the part of the controller that
implements primitives, but we presume that these
instructions make use of primitive arithmetic operations in
the machine's data paths. Consider how much less code would
be generated if the compiler could open-code
primitives---that is, if it could generate code to directly
use these primitive machine operations. The expression "(+ a
1)" might be compiled into something as simple as

(assign val (op lookup-variable-value) (const a) (reg env))
(assign val (op +) (reg val) (const 1))

In this exercise we will extend our compiler to support open
coding of selected primitives. Special-purpose code will be
generated for calls to these primitive procedures instead of
the general procedure-application code. In order to support
this, we will augment our machine with special argument
registers "arg1" and "arg2". The primitive arithmetic
operations of the machine will take their inputs from "arg1"
and "arg2". The results may be put into "val", "arg1", or
"arg2".

The compiler must be able to recognize the application of an
open-coded primitive in the source program. We will augment
the dispatch in the "compile" procedure to recognize the
names of these primitives in addition to the reserved words
(the special forms) it currently recognizes. For each
special form our compiler has a code generator. In this
exercise we will construct a family of code generators for
the open-coded primitives.

a. The open-coded primitives, unlike the special forms, all
need their operands evaluated. Write a code generator
"spread-arguments" for use by all the open-coding code
generators. "spread-arguments" should take an operand list
and compile the given operands targeted to successive
argument registers. Note that an operand may contain a call
to an open-coded primitive, so argument registers will have
to be preserved during operand evaluation.

b. For each of the primitive procedures "=", "*", "-", and
"+", write a code generator that takes a combination with
that operator, together with a target and a linkage
descriptor, and produces code to spread the arguments into
the registers and then perform the operation targeted to the
given target with the given linkage. You need only handle
expressions with two operands. Make "compile" dispatch to
these code generators.

c. Try your new compiler on the "factorial" example. Compare
the resulting code with the result produced without open
coding.

d. Extend your code generators for "+" and "*" so that they
can handle expressions with arbitrary numbers of operands.
An expression with more than two operands will have to be
compiled into a sequence of operations, each with only two
inputs.

|#

(load-ex "5.33") ; skip 5.36 & 5.37 which were experimental

#| Answer 

A more final implementation needs to define more information about the
open-coded primitives such as whether they accept zero or one arguments, and how
multiple arguments can be handled.

It is much more efficient. For example, compare the = call of the two factorials.

This

  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch34))
compiled-branch33
  (assign continue (label after-call32))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch34
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call32

simplifies to this

  (assign arg1 (op lookup-variable-value) (const n) (reg env))
  (assign arg2 (const 1))
  (assign val (op =) (reg arg1) (reg arg2))

|#

;;; rewrite from 5.33
(define (comp exp target linkage)
  (cond [(self-evaluating? exp)
         (compile-self-evaluating exp target linkage)]
        [(quoted? exp) (compile-quoted exp target linkage)]
        [(variable? exp)
         (compile-variable exp target linkage)]
        [(assignment? exp)
         (compile-assignment exp target linkage)]
        [(definition? exp)
         (compile-definition exp target linkage)]
        [(if? exp) (compile-if exp target linkage)]
        [(lambda? exp) (compile-lambda exp target linkage)]
        [(begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage)]
        [(cond? exp) (comp (cond->if exp) target linkage)]
        [(ocp? exp)
         (compile-ocp exp target linkage)]
        [(application? exp)
         (compile-application exp target linkage)]
        [else
         (error "compile" "unknown expression type" exp)]))

(define open-coded-primitives '(= * - +))

(define (ocp? exp) 
  (and (pair? exp) (member (car exp) open-coded-primitives)))

;;; rewrite of 5.33 -- compile-application
(define (compile-ocp exp target linkage)
  (do-one (operator exp) (operands exp) target linkage))

(define (do-one ocp operands target linkage)
  (if (null? operands)
      (compile-ocp-call ocp '() target linkage) ; zero arguments
      (let ([one (comp (car operands) 'arg1 'next)])
        (preserving '(continue env)
          one
          (do-two ocp (cdr operands) target linkage)))))
(define (do-two ocp operands target linkage)
  (if (null? operands)
      (compile-ocp-call ocp '(arg1) target linkage) ; one arguments
      (let ([two (comp (car operands) 'arg2 'next)])
        (preserving '(arg1 continue env)
          two
          (do-n ocp (cdr operands) target linkage)))))
(define (do-n ocp operands target linkage)
  (if (null? operands)
      (compile-ocp-call ocp '(arg1 arg2) target linkage) ; final two arguments
      (let ([next (comp (car operands) 'arg2 'next)])
        (preserving '(continue env)
          (compile-ocp-call ocp '(arg1 arg2) 'arg1 'next)
          (preserving '(arg1)
            next
            (do-n ocp (cdr operands) target linkage))))))

;;; rewrite of 5.33 -- compile-procedure-call
(define (compile-ocp-call ocp input-regs target linkage)
 (end-with-linkage linkage
  (make-instruction-sequence input-regs (list target)
    (list (append `(assign ,target (op ,ocp))
                  (map (lambda (r) (list 'reg r)) input-regs))))))

;;; re-write of 5.33
(define all-regs '(env proc val arg1 arg2 argl continue))

#| Tests |#
(define-test (compile-test '(+ 1 2 3))
             6)