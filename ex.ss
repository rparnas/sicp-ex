
#| Utilities |#

; Loads an exercise given its string name in a format like
; "2.56". If the optional second argument is provided as #t,
; the name of the exercise to be loaded is written to the
; file "n", the interpreter exits, the interpreter is run
; fresh, and the startup script below is responsible for
; loading the exercise.
(define (load-ex n . args)
  (define (ex-n-to-path n)
    (format ".\\exercises\\~a.ss" n))
  (define (write-n n)
    (with-output-to-file "n" (lambda () (write n))))
  (cond [(= (length args) 0)
         ; load an exercise file
         (let ([path (ex-n-to-path n)])
           (load path)
           (display (format "Loaded ~s" path))
           (newline))]
        [(= (length args) 1)
         ; reset the interpreter and load an exercise file.
         (cond [(not (string? n)) 
                (error "load-ex" "n must be a string" n)]
               [(not (file-exists? (ex-n-to-path n)))
                (error "load-ex" "invalid exercise" n)]
               [else 
                (write-n n)
                (exit)])]
        [else 
         (error "load-ex" "too many arguments" (cons n args))]))

; increments the given number.
(define (inc x) 
  (+ x 1))

; decrements the given number.
(define (dec x) 
  (- x 1))

; squares the given number
(define (square x)
  (* x x))

; displays a functions arguments when it is invoked
(define (tracize f)
  (lambda args
    (display (format "~a\r\n" (cons f args)))
    (apply f args)))

#| Startup |#

; Loads an exercise file if there is a file called "n"
; present that contains the name of the exercise to load.
((lambda ()
  (define (read-n)
    (if (not (file-exists? "n"))
        '()
        (let ([path (with-input-from-file "n" read)])
              (delete-file "n")
              path)))
  (let ([n (read-n)])
    (if (not (null? n))
        (load-ex n)
        (void)))))