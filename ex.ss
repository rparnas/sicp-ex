; Given an exercise number returns where that exercise is stored.
(define (ex-n-to-path n)
  (define exercise-dir ".\\exercises\\")
  (define exercise-ext ".ss")
  (string-append exercise-dir n exercise-ext))

; Restarts scheme entirely and loads the given exercise file.
(define (ex n)
  (define (write-n n)
    (with-output-to-file "n" (lambda () (write n)) 'replace))
  (cond [(not (string? n)) 
          (error "ex" "n must be a string" n)]
        [(not (file-exists? (ex-n-to-path n)))
          (error "ex" "n must be a valid exercise" n)]
        [else 
          (begin 
            (write-n n)
            (exit))]))

; Loads an exercise file and displays it so the user can keep track of what was loaded.
(define (load-ex n)
  (let ([path (ex-n-to-path n)])
    (begin
      (load path)
      (display (string-append "Loaded " path "\n")))))

; Startup script
((lambda ()
  (define (read-n)
    (if (not (file-exists? "n"))
        '()
        (let ([path (with-input-from-file "n" read)])
              (delete-file "n")
              path)))
  (let ([n (read-n)])
    (if (not (null? n))
        (load-ex n)))))