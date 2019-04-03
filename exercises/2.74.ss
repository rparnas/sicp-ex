#|

Exercise 2.74: Insatiable Enterprises, Inc., is a highly
decentralized conglomerate company consisting of a large
number of independent divisions located all over the world.
The company's computer facilities have just been
interconnected by means of a clever network-interfacing
scheme that makes the entire network appear to any user to
be a single computer. Insatiable's president, in her first
attempt to exploit the ability of the network to extract
administrative information from division files, is dismayed
to discover that, although all the division files have been
implemented as data structures in Scheme, the particular
data structure used varies from division to division. A
meeting of division managers is hastily called to search for
a strategy to integrate the files that will satisfy
headquarters' needs while preserving the existing autonomy
of the divisions.

Show how such a strategy can be implemented with
data-directed programming. As an example, suppose that each
division's personnel records consist of a single file, which
contains a set of records keyed on employees' names. The
structure of the set varies from division to division.
Furthermore, each employee's record is itself a set
(structured differently from division to division) that
contains information keyed under identifiers such as
"address" and "salary". In particular:

a. Implement for headquarters a "get-record" procedure that
retrieves a specified employee's record from a specified
personnel file. The procedure should be applicable to any
division's file. Explain how the individual divisions' files
should be structured. In particular, what type information
must be supplied?

b. Implement for headquarters a "get-salary" procedure that
returns the salary information from a given employee's
record from any division's personnel file. How should the
record be structured in order to make this operation work?

c. Implement for headquarters a "find-employee-record"
procedure. This should search all the divisions' files for
the record of a given employee and return the record. Assume
that this procedure takes as arguments an employee's name
and a list of all the divisions' files.

d. When Insatiable takes over a new company, what changes
must be made in order to incorporate the new personnel
information into the central system?

|#

#| Answer 

a. The files internal structure is hidden as it is handed
to HQ, HQ must remember the type of the file.

b. For get-salary, each reacord must be tagged with the type
of database it game from. It is convienient to modify  get-
record to remember this information. Alternatively, each
department could right a "is-my-type" function but this is
not robust if two departments have indistinguishable
formats.

c. Implemented.

d. When Insatiable takes over a new company, the new
department must simply register the mandated selectors (get-
file, get-record, get-salary etc).

NOTE: If a get-name selector was desired by hq it could be a
big pain as it is not specified that an individual record
has to have a key in it. The best strategy would be to wrap
the raw record at the top level and remember the name as you
retrieved it, similar to what is done with files.

|#
(load-ex "2.73")

(define (get-file type)
  (attach-tag type ((get 'get-file type))))

(define (get-record employee-name personnel-file)
  (let ([record ((get 'get-record (type-tag personnel-file)) employee-name)])
    (if record
        (attach-tag (type-tag personnel-file) record)
        #f)))

(define (get-salary r) (apply-generic 'get-salary r))

(define (find-employee-record employee-name files)
  (if (null? files)
      #f
      (let ([record (get-record employee-name (car files))])
        (if (not record)
            (find-employee-record employee-name (cdr files))
            record))))

#| Tests |#
(define (install-accounting)
  ;; internal
  (define (make-record name address salary)
    (list name address salary))
  (define (get-name record) (car record))
  (define (get-address record) (cadr record))
  (define (get-salary record) (caddr record))
  (define file (list (make-record 'alan "1st St" 100)
                     (make-record 'brad "2nd St" 200)
                     (make-record 'chad "3rd St" 300)))
  ;; interface
  (put 'get-file 'accounting (lambda() file))
  (put 'get-record 'accounting (lambda (name)
                                 (find (lambda (r) (eq? (get-name r) name)) file)))
  (put 'get-salary '(accounting) get-salary)
  (put 'get-name '(accounting) get-name)
  'done)

(define (install-engineering)
  ;; internal
  (define file (make-eq-hashtable))
  (define (record-set! name address salary)
    (let ([values (make-eq-hashtable)])
      (hashtable-set! values 'address address)
      (hashtable-set! values 'salary salary)
      (hashtable-set! file name values)))
  (define (get-address-get record)
    (hashtable-ref record 'address #f))
  (define (get-salary record)
    (hashtable-ref record 'salary #f))
  (record-set! 'dirk "4th St" 130)
  (record-set! 'edie "5th St" 140)
  (record-set! 'foxe "6th St" 150)
  ;; interface
  (put 'get-file 'engineering (lambda () file))
  (put 'get-record 'engineering (lambda (name)
                                  (let ([ret (hashtable-ref file name #f)])
                                    (if ret
                                        (cons name ret)
                                        ret))))
  (put 'get-salary '(engineering) (lambda (wrapped)
    (get-salary (cdr wrapped))))
  (put 'get-name '(engineering) (lambda (wrapped)
    (car wrapped)))
  'done)

#| Tests |#
(install-accounting)
(install-engineering)

(define accounting-file (get-file 'accounting))
(define engineering-file (get-file 'engineering))
(define all-files (list accounting-file engineering-file))

(define-test (not (get-record 'alan accounting-file))
             #f)
(define-test (not (get-record 'dirk engineering-file))
             #f)
(define-test (get-record 'zorg accounting-file) 
             #f)
(define-test (get-record 'zorg engineering-file)
             #f)
(define-test (get-salary (get-record 'alan accounting-file))
             100)
(define-test (get-salary (get-record 'dirk engineering-file))
             130)

(define-test (find-employee-record 'alan all-files)
             '(accounting alan "1st St" 100))
(define-test (find-employee-record 'zorg all-files)
             #f)

#| Extra Tests |#
(define (get-name r) (apply-generic 'get-name r))
(define-test (get-name (get-record 'alan accounting-file))
             'alan)
(define-test (get-name (get-record 'dirk engineering-file))
             'dirk)
