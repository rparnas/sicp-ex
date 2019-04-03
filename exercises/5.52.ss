#|

Exercise 5.52: As a counterpoint to Exercise 5.51, modify
the compiler so that it compiles Scheme procedures into
sequences of C instructions. Compile the metacircular
evaluator of Section 4.1 to produce a Scheme interpreter
written in C.

|#

#| Answer

The instruction set of our register machine simulator is somewhat simple, so it
seems best to compile and then do a syntactic transformation of the compiled
code.

Based on unit tests, I copied several examples of compiled programs from
regresssion tests into the C project.

Issues to follow up on are:

  * How should primitive procedures be dealt with? Perhaps every single 
    "primitive" procedure should correspond to a C function that can be accessed
    directly by the compiler.

|#

(load-ex "5.40")

#| Answer -- utilities |#

(define (accumulate op initial sequence) 
  (if (null? sequence) 
   initial 
   (op (car sequence) 
     (accumulate op initial (cdr sequence))))) 

(define (flatmap proc seq) 
  (accumulate append '() (map proc seq)))

(define (string-join sep ls)
  (cond [(null? ls) ""]
        [(null? (cdr ls)) (car ls)]
        [(string-append (car ls) sep (string-join sep (cdr ls)))]))

#| Answer |#

(define (to-name name)
  (cond [(eq? name 'continue) "cont"]
        [(eq? name '=) "number_equals"]
        [(eq? name '*) "number_multiply"]
        [(eq? name '-) "number_minus"]
        [(eq? name 'list) "list1"] ;;; TODO: Very dangerous / FIX
        [else
          (list->string (flatmap 
                           (lambda (c)
                             (cond [(eq? c #\-) '(#\_)]
                                   [(eq? c #\?) '(#\p)]
                                   [(eq? c #\!) '(#\_ #\b #\a #\n #\g)]
                                   [else (list c)]))
                         (string->list (symbol->string name))))]))

(define (to-sexp exp)
  (cond [(null? exp) "MakeNull()"]
        [(symbol? exp) (string-append "MakeSymbol(\"" (to-name exp) "\")")]
        [(number? exp) (string-append "MakeNumber(" (number->string exp) ")")]
        [(pair? exp)
         (string-append "MakePair(" (to-sexp (car exp)) ", " (to-sexp (cdr exp)) ")")]
        [else (error "to-sexp" "not implemented" exp)]))

(define (to-number-label exp)
  (cond [(symbol? exp)
         (add-cenum-value! (to-name exp))
         (string-append "MakeNumber(" "(" cenum ")" (to-name exp) ")")]
        [else (error "to-label" "not implemented" exp)]))

(define (instruction->C i)
  (cond [(symbol? i)
         (string-append "  " (to-name i) ":\n")]
        [(eq? (car i) 'assign)
         (add-assign-reg! (to-name (cadr i)))
         (string-append 
            "    " 
            (to-name (cadr i))
            " = "
            (assign-rest->C (cddr i)) 
            ";\n")]
        [(or (eq? (car i) 'goto) (eq? (car i) 'branch))
         (string-append "    goto " (goto-term->C (cadr i)) ";\n")]
        [(eq? (car i) 'perform)
         (string-append (assign-rest->C (cdr i)) ";\n" )]
        [(eq? (car i) 'test)
         (string-append "    if(GetBoolValue(" (assign-rest->C (cdr i)) "))\n")]
        [(eq? (car i) 'save)
         (string-append "    save(" (to-name (cadr i)) ");\n")]
        [(eq? (car i) 'restore)
         (string-append "    " (to-name (cadr i)) " = restore();\n")]
        [else (string-append "  /* TODO */ " "\n")]))

(define (assign-rest->C rest)
  (define (term->C i)
    (cond [(eq? (car i) 'reg)
           (to-name (cadr i))]
          [(eq? (car i) 'const)
           (to-sexp (cadr i))]
          [(eq? (car i) 'op)
           (to-name (cadr i))]
          [(eq? (car i) 'label)
           (to-number-label (cadr i))]
          [else (error "term->C" "not implemented" i)]))
  (if (null? rest)
      (error "rest->C" "expected at least one term"))
  (let ([one (term->C (car rest))])
    (if (eq? (caar rest) 'op)
        (string-append
          one
          "("
          (string-join ", " (map term->C (cdr rest)))
          ")"
        )
        (if (null? (cdr rest))
            one
            (error "rest->C" "invalid" rest)))))

(define (goto-term->C term)
  (cond [(eq? (car term) 'label)
         (to-name (cadr term))]
        [(eq? (car term) 'reg)
         (add-jump-reg! (to-name (cadr term)))
         (string-append (to-name (cadr term)) "_")]
        [else (error "goto-term->C" "not implemented" term)]))

;;; state variables -- hacky but I didn't want to pass things around.
(define cenum "?")
(define cenum-values '())
(define jump-regs '())
(define assign-regs '())
(define (add-cenum-value! sym)
  (if (not (member sym cenum-values))
      (set! cenum-values (cons sym cenum-values))))
(define (add-jump-reg! reg)
    (if (not (member reg jump-regs))
      (set! jump-regs (cons reg jump-regs))))
(define (add-assign-reg! reg)
  (if (and (not (member reg assign-regs)) (not (equal? reg "cont")) (not (equal? reg "env")))
      (set! assign-regs(cons reg assign-regs))))

(define (compiled->C prog-name instructions)
  (define done (make-label 'done))
  (define doneStr (symbol->string done))
  (set! cenum (string-append (to-name prog-name) "_continues"))
  (set! cenum-values (list doneStr))
  (set! jump-regs '("cont"))
  (set! assign-regs '())
  (let ([body (map instruction->C instructions)])
    (append
      (list 
        (string-append "typedef enum { " (string-join ", " cenum-values) " } " cenum  ";\n")
        (string-append "static Sexp " (to-name prog-name) "()\n")
        "{\n"
        (string-append "  Sexp cont = MakeNumber((" cenum ")" doneStr ");\n")
        "  Sexp env = setup_environment();\n"
        (string-join "\n"
                     (map (lambda (reg) 
                            (string-append "  Sexp " reg " = MakeNull();")) 
                          assign-regs))
        "\n\n")
      body
      (list
        (string-append "  goto " doneStr ";\n"))
      (flatmap (lambda (reg)
                 (list 
                   (string-append "  " reg "_:\n")
                   (string-append "  switch (GetNumberValue(" reg "))\n")
                   "  {\n"
                   (string-join "\n" (map (lambda (x) (string-append "    case " x ": goto " x ";")) cenum-values))
                   "\n  }\n"))
               jump-regs)
      (list
        (string-append "  " doneStr ":\n")
        "    return val;\n")
      (list "}\n"))))

(define (compc prog-name exp)
  ;;; TODO: I'd like to reset the counter, but enums don't allow the same
  ;;; name to be defined in two different enums.
  ; (set! label-counter 0)
  (compiled->C prog-name (caddr (comp exp 'val 'next '()))))
