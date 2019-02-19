#| Compatability Hacks for Chez Scheme / Racket |#
(define interpreter "?")

(define (safe-directory-separator)
  (cond [(equal? interpreter "scheme") (directory-separator)]
        [else #\\]))

(define (safe-sort pred ls)
  (cond [(equal? interpreter "racket") (sort ls pred)]
        [else (sort pred ls)]))

(define (safe-try thunk)
  (cond [(equal? interpreter "racket")
         (call/cc (lambda (k) (call-with-exception-handler (lambda (e) (k e)) thunk)))]
        [else
         (call/cc (lambda (k) (with-exception-handler (lambda (e) (k e)) thunk)))]))

#| Exercise I/O |#

;;; Stack of exercises being loaded.
(define current-ex "none")

;;; Extension of exercise files in the folder ex-root.
(define ex-ext "ss")

;;; Folder containing exercise files.
(define ex-root (format ".~aexercises~a" (safe-directory-separator) (safe-directory-separator)))

;;; Converts exercise to path like "1.10" is ".\\exercises\1.10.ss"
(define (ex->path ex)
  (format "~a~a.~a" ex-root ex ex-ext))

#| Exercise Name Parsing |#

;;; Given an exercise string like "1.10" returns a list like (1 10) or #f if the given is not valid.
(define (ex->list ex)
  (if (not (string? ex))
      #f
      (let* ([ls (string->list ex)]
             [dec (member #\. ls)])
        (if (not dec)
            #f
            (let ([section (string->number (substring ex 0 (- (string-length ex) (length dec))))]
                  [number (string->number (list->string (cdr dec)))])
              (and (integer? section) 
                   (integer? number) 
                   (> section 0)
                   (> number 0)
                   (list section number)))))))

;;; True if the 1st exercise is less than the 2nd like "2.1" < "2.10" is true.
(define (ex-lt ex0 ex1)
  (or (< (ex-section ex0) (ex-section ex1))
      (and (= (ex-section ex0) (ex-section ex1))
           (< (ex-number ex0) (ex-number ex1)))))

;;; Returns the number of the given exercise like "1.10" is 10.
(define (ex-number ex)
  (cadr (ex->list ex)))

;;; Returns the section of the given exercise like "1.10" is 1.
(define (ex-section ex)
  (car (ex->list ex)))

;;; Returns true if the given exercise is valid.
(define (is-ex-valid? ex) 
  (not (not (ex->list ex))))

#| Testing |#
;;; Sorted alist with one test-set for each loaded exercise, ordered by
;;; exercise. A test set has a string exercise name, a list of flags which are
;;; symbols, and a list of tests in the form (expression . expected-result).
;;; like (("1.1" (flag) ((+ 2 2) 4)) ("1.2" () ((+ 3 3) 6)))
(define all-tests '())
(define (make-test-set ex flags tests) (append (list ex flags) tests))
(define (test-set-ex test-set) (car test-set))
(define (test-set-flags test-set) (cadr test-set))
(define (test-set-tests test-set) (cddr test-set))
(define (test-expression test) (car test))
(define (test-expected-result test) (cdr test))

;;; Gets the test set for the given exercise.
(define (get-test-set ex)
  (or (assoc ex all-tests) (make-test-set ex '() '())))

;;; Sets the test-set for the given exercise.
(define (set-test-set ex test-set)
  (set! all-tests
        (safe-sort (lambda (ts0 ts1) (ex-lt (test-set-ex ts0) (test-set-ex ts1)))
                   (cons test-set
                         (filter (lambda (test-set) 
                                   (not (equal? ex (test-set-ex test-set))))
                                 all-tests)))))

;;; Adds a flag for the given exercise
(define (add-flag ex flag)
  (let ([test-set (get-test-set ex)])
    (set-test-set ex (make-test-set (test-set-ex test-set)
                                    (append (test-set-flags test-set) (list flag))
                                    (test-set-tests test-set)))))
;;; Adds a test for the given exercise
(define (add-test ex expression expected-result)
  (let ([test-set (get-test-set ex)])
    (set-test-set ex (make-test-set (test-set-ex test-set)
                                    (test-set-flags test-set)
                                    (append (test-set-tests test-set)
                                            (list (cons expression expected-result)))))))

;;; Flags the currently loading exercise as breaking compatibility with regression tests.
(define (no-regression)
  (add-flag (car current-ex) 'no-regression))

;;; Runs the given test and returns (pass? expression expected-result result)
;;; like (run-test '(+ 2 2) 4) is (#t (+ 2 2) 4 4) or
;;; like (run-test '(+ 2 2) 5) is (#f (+ 2 2) 4 5) or
;;; like (run-test '(+ + +) 4) is (#f (+ + +) 4 #<compound condition>)
(define (run-test expression expected-result)
  (let ([result (safe-try (lambda () (eval expression)))])
    (list (equal? expected-result result)
          expression
          expected-result
          result)))

;;; Runs any loaded tests for the given exercise and all previous exercises.
(define (run-tests ex)
  (let* ([test-set (get-test-set ex)]
         [sets-to-run (if (member 'no-regression (test-set-flags test-set))
                          (list test-set)
                          (filter (lambda (test-set)
                                    (or (ex-lt (test-set-ex test-set) ex)
                                    (equal? ex (test-set-ex test-set))))
                                  all-tests))])
    (map (lambda (test-set)
           (cons (test-set-ex test-set)
                 (map (lambda (test)
                        (run-test (test-expression test) (test-expected-result test)))
                      (test-set-tests test-set))))
          sets-to-run)))   

;;; Transforms a list of results into a summary string.
(define (test-results->summary ex results)
  (define (count-pass results)
    (apply + (map (lambda (results-for-one-ex) 
                    (length (filter car (cdr results-for-one-ex)))) 
                  results)))
  (define (count-total results)
    (apply + (map (lambda (results-for-one-ex) 
                    (length (cdr results-for-one-ex))) 
                  results)))
  (define (test-noun count)
    (if (= count 1) "test" "tests"))
  (let* ([this-results (filter (lambda (results-for-one-ex) (equal? (car results-for-one-ex) ex)) results)]
         [reg-results (filter (lambda (results-for-one-ex) (not (equal? (car results-for-one-ex) ex))) results)]
         [this-pass (count-pass this-results)]
         [this-total (count-total this-results)]
         [reg-pass (count-pass reg-results)]
         [reg-total (count-total reg-results)]
         [this-summary (format "~a/~a ~a passed" this-pass this-total (test-noun this-pass))]
         [reg-summary (format "~a/~a regression ~a passed" reg-pass reg-total (test-noun reg-pass))])
    (cond [(and (= this-total 0) (= reg-total 0))
           ""]
          [(= this-total 0)
           reg-summary]
          [(= reg-total 0)
           this-summary]
          [else 
           (format "~a, ~a" this-summary reg-summary)])))

#| Public Utilities |#

;;; Syntax for adding a test like (define-test (+ 2 2) 4)
(define-syntax define-test
  (syntax-rules ()
    [(_ expression expected-result)
     (add-test (car current-ex) 'expression expected-result)]))

;;; decrements the given number by 1.
(define (dec n) 
  (- n 1))

;;; increments the given number by 1.
(define (inc n) 
  (+ n 1))

;;; Given the name of an exercise like "2.56" loads the corresponding
;;; exercise file and runs any automated tests.
(define (load-ex ex)
  (if (or (not (is-ex-valid? ex))
          (not (file-exists? (ex->path ex))))
      (error "load-ex" "invalid exercise" ex)
      (begin
        (set! current-ex (cons ex current-ex))
        (set-test-set ex (make-test-set ex '() '()))
        (load (ex->path ex))
        (display (format "Loaded ~a" (ex->path ex)))
        (let* ([test-results (run-tests ex)]
               [test-summary (test-results->summary ex test-results)])
          (if (equal? test-summary "")
              (void)
              (display (format " (~a)" test-summary))))
        (set! current-ex (cdr current-ex))
        (newline))))

;;; Given the name of an exercise like "2.56", exits the interpreter, re-opens
;;; the interpreter, and then loads that exercise's corresponding file.
(define (reset-ex ex)
  (if (or (not (is-ex-valid? ex))
          (not (file-exists? (ex->path ex))))
      (error "reset-ex" "invalid exercise" ex)
      (begin
        (with-output-to-file "ex-to-load" (lambda () (write ex)))
        (exit))))

;;; squares the given number
(define (square x)
  (* x x))

;;; displays a function's name and arguments when it is invoked
(define (tracize f)
  (lambda args
    (display (format "~a" (cons f args)))
    (newline)
    (apply f args)))

#| Startup Script |#

;;; Load an exercise file if there is a file called "ex-to-load" present that
;;; contains the name of the exercise to be loaded.
((lambda ()
   (set! interpreter (with-input-from-file "interpreter" read))
   (delete-file "interpreter")
   (if (not (file-exists? "ex-to-load"))
       (void)
       (let ([ex (with-input-from-file "ex-to-load" read)])
         (delete-file "ex-to-load")
         (load-ex ex)
         (void)))))
