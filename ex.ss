
#| Utilities |#

; Loads an exercise given as a string in a format like
; "2.56". If the optional second argument is provided as #t,
; the interpreter is reset beforehand.
(define (load-ex n . args)
  (define (ex-n-to-path n)
    (define exercise-dir ".\\exercises\\")
    (define exercise-ext ".ss")
    (string-append exercise-dir n exercise-ext))
  (define (write-n n)
    (with-output-to-file "n" (lambda () (write n))))
  (cond [(= (length args) 0)
         ; load an exercise file
         (let ([path (ex-n-to-path n)])
           (load path)
           (display (string-append "Loaded " path "\n")))]
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