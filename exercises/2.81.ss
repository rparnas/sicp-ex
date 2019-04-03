#|

Exercise 2.81: Louis Reasoner has noticed that
"apply-generic" may try to coerce the arguments to each
other's type even if they already have the same type.
Therefore, he reasons, we need to put procedures in the
coercion table to coerce arguments of each type to their own
type. For example, in addition to the
"scheme-number->complex" coercion shown above, he would do:

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

a. With Louis's coercion procedures installed, what happens
if "apply-generic" is called with two arguments of type
"scheme-number" or two arguments of type "complex" for an
operation that is not found in the table for those types?
For example, assume that we've defined a generic
exponentiation operation:

(define (exp x y) (apply-generic 'exp x y))

and have put a procedure for exponentiation in the
Scheme-number package but not in any other package:

 ;; following added to Scheme-number package 
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))
      ; using primitive "expt" 

What happens if we call "exp" with two complex numbers as
arguments?

b. Is Louis correct that something had to be done about
coercion with arguments of the same type, or does
"apply-generic" work correctly as is?

c. Modify "apply-generic" so that it doesn't try coercion if
the two arguments have the same type.

|#

(load-ex "2.80")

#| Code the book assumes we have |#
(define coercion-table (list))

(define (put-coercion t0 t1 proc)
  (set! coercion-table (cons (list (list t0 t1) proc)
                             coercion-table)))

(define (get-coercion t0 t1)
  (let ([ret (find (lambda (x)
                     (equal? (list t0 t1) (car x)))
                   coercion-table)])
    (if ret
        (cadr ret)
        ret)))

#| Code from book |#
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 
              'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

#| Answer

a. apply-generic enters infinite recursion because it will
try to untangle things by coercing argument 1 to the type of
argument 2, but since both arguments are complex it we are
back where we started.

b. apply-generic kind of works as is without Louis's same
type coercions it just returns an error if a procedure isn't
found to deal with the two arguments of the same type.
However the specification does not say "coercions of the
same type aren't allowed" thus apply-generic should this
case.

c. apply-generic doesn't try coercion if the two arguments
have the same type.

|#

; Keep the old apply generic around in case you want to
; retest a. or b.
(define (apply-generic-ab) apply-generic)

(define (apply-generic op . args)
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)]
         [fail (lambda ()
                 (error "apply-generic" "no method for" (list op type-tags)))])
    (cond [proc 
           (apply proc (map contents args))]
          [(= (length args) 2)
           (let* ([type1 (car type-tags)]
                  [type2 (cadr type-tags)]
                  [a1 (car type-tags)]
                  [a2 (cadr type-tags)]
                  [t1->t2 (get-coercion type1 type2)]
                  [t2->t1 (get-coercion type2 type1)])
             (cond [(equal? type1 type2)
                    (fail)]
                   [t1->t2 
                    (apply-generic op (t1->t2 a1) a2)]
                   [t2->t1
                    (apply-generic op a1 (t2->t1 a2))]
                   [else (fail)]))]
            [else (fail)])))
#| Tests |#

;; Don't install except for testing to avoid poluting the
;; exercises.
(define (test-a-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)
  (put-coercion 'scheme-number
                'scheme-number
                scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex)
  (put 'exp '(scheme-number scheme-number)
    (lambda (x y) (tag (expt x y))))
  'done)

(define (test-b-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'exp '(scheme-number scheme-number)
    (lambda (x y) (tag (expt x y))))
  'done)

#|

; a.
> (test-a-package)
done
> (define (exp x y) (apply-generic-ab 'exp x y))
> (exp (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
. . . hangs

; b. assumes fresh environment
> (test-b-package)
'done
> (define (exp x y) (apply-generic-ab 'exp x y))
> (exp (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
Exception: No method for these types with irritant (exp (complex complex))
Type (debug) to enter the debugger.

; c. assumes fresh enviornment
> (test-a-package)
> (define (exp x y) (apply-generic 'exp x y))
> (exp (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
Exception in apply-generic: no method for with irritant (exp (complex complex))
Type (debug) to enter the debugger.
>

|#

