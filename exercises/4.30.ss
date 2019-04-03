#|

Exercise 4.30: Cy D. Fect, a reformed C programmer, is
worried that some side effects may never take place, because
the lazy evaluator doesn't force the expressions in a
sequence. Since the value of an expression in a sequence
other than the last one is not used (the expression is there
only for its effect, such as assigning to a variable or
printing), there can be no subsequent use of this value
(e.g., as an argument to a primitive pro
cedure) that will
cause it to be forced. Cy thus thinks that when evaluating
sequences, we must force all expressions in the sequence
except the final one. He proposes to modify "eval-sequence"
from Section 4.1.1 to use "actual-value" rather than "eval":

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

a. Ben Bitdiddle thinks Cy is wrong. He shows Cy the
"for-each" procedure described in Exercise 2.23, which gives
an important example of a sequence with side effects:

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

He claims that the evaluator in the text (with the original
"eval-sequence") handles this correctly:

 ;;; L-Eval input: 
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
 57 
 321 
 88 
 ;;; L-Eval value: 
 done 

Explain why Ben is right about the behavior of "for-each".

b. Cy agrees that Ben is right about the "for-each" example,
but says that that's not the kind of program he was thinking
about when he proposed his change to "eval-sequence". He
defines the following two procedures in the lazy evaluator:

(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

What are the values of "(p1 1)" and "(p2 1)" with the
original "eval-sequence"? What would the values be with Cy's
proposed change to "eval-sequence"?

c. Cy also points out that changing "eval-sequence" as he
proposes does not affect the behavior of the example in part
a. Explain why this is true.

d. How do you think sequences ought to be treated in the
lazy evaluator? Do you like Cy's approach, the approach in
the text, or some other approach?

|#

#| Answer 

a. Assuming display isn't a primitive procedure, Ben is right about for-each
because the procedure display is forced as it is the operation of an
application. The lazy argument passed to display is forced at some point by the
display procedure thus everything we wrote gets evaulated.

;;; original from 4.1
(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))

b. With the original eval-sequence (p1 1) ==> (1 2). The set! expresssion is not
delayed here because it is a special form. (p2 1) ==> 1. The set! expression
never gets evaluated because it is passed as an argument in an application, and
that argument is only referenced in the head of the sequence inside the
operator. Changing eval-sequence does not affect p1 but it causes (p2 1) ==> (1
2) because now all elements in a sequence are forced.

c. Changing eval-sequence does not affect the example in part a. because the
procedures used in the for-each happen to force the arguments in the sequence
even if the eval-sequence itself does not.

d. If the general idea of lazy is "force values only when needed" the new eval-
sequence here seems to violate that principle. This solution for an issue
related to side-effects is too invasive.

|#
