#|

Exercise 4.43: Use the "amb" evaluator to solve the
following puzzle:

Mary Ann Moore's father has a yacht and so has each of his
four friends: Colonel Downing, Mr. Hall, Sir Barnacle Hood,
and Dr. Parker. Each of the five also has one daughter and
each has named his yacht after a daughter of one of the
others. Sir Barnacle's yacht is the Gabrielle, Mr. Moore
owns the Lorna; Mr. Hall the Rosalind. The Melissa, owned by
Colonel Downing, is named after Sir Barnacle's daughter.
Gabrielle's father owns the yacht that is named after Dr.
Parker's daughter. Who is Lorna's father?

Try to write the program so that it runs efficiently (see
Exercise 4.40). Also determine how many solutions there are
if we are not told that Mary Ann's last name is Moore.

|#

(load-ex "4.42")

#| Answer 

Colonel Downing is Lorna's father. 

There are two solutions sets. In one Colnel Downing is Lorna's father. In the
other Dr. Parker is Lorna's father.

|#

(define setup-environment-442 setup-environment)
(define (setup-environment)
  (let ([env (setup-environment-442)])
    (define (add exp) (ambeval exp 
                               env 
                               (lambda (value next) (void))
                               (lambda () (error "setup-environment" "fail!"))))

    ;;; adding apply to interpeter to implement full-featured map seems overkill. 
    (add '(define (map1 p l1)
        (if (or (null? l1))
            (list)
            (cons (p (car l1))
                  (map1 p (cdr l1))))))
    (add '(define (map2 p l1 l2)
            (if (or (null? l1) (null? l2))
                (list)
                (cons (p (car l1) (car l2))
                      (map2 p (cdr l1) (cdr l2))))))
    (add '(define (assoc obj alist)
            (cond [(null? alist) 'false]
                  [(equal? (car (car alist)) obj) (car alist)]
                  [else (assoc obj (cdr alist))])))
    (add '(define (assoc-tail obj alist)
            (cond [(null? alist) 'false]
                  [(equal? (cdr (car alist)) obj) (car alist)]
                  [else (assoc-tail obj (cdr alist))])))
    env))

#| Tests -- infrastructure |#

(define-test (eval-one '(map1 (lambda (x) (+ x 1)) '(1 2 3 4 5)))
             '(2 3 4 5 6))

(define-test (eval-one '(assoc 'b '((a . 1) (b . 2) (c . 3))))
             '(b . 2))

(define-test (eval-one '(assoc-tail 'z '((a . d) (b . z) (c . x))))
             '(b . z))

(define-test (eval-one '(member '(b . z) '((a . d) (b . z) (c . x))))
             'true)

#| Tests |#

;;; original problem
(define-test (eval-all
  '(let* ([fathers '(downing hall hood moore parker)]
          [daughters '(gabrielle lorna mary-ann melissa rosalind)]
          [f->d (map2 cons fathers (permutations daughters))]
          [f->y (map2 cons fathers (permutations daughters))])
    (define (fd father daughter) (require (member (cons father daughter) f->d)))
    (define (fy father yacht) (require (member (cons father yacht) f->y)))
    ;;; requirements
    (fd 'moore 'mary-ann)  ; Mary Ann Moore
    (fy 'hood 'gabrielle)  ; Sir Barnacle's yacht is the Gabrielle,
    (fy 'moore 'lorna)     ; Mr. Moore owns the Lorna;
    (fy 'hall 'rosalind)   ; Mr. Hall the Rosalind.
    (fy 'downing 'melissa) ; The Melissa, owned by Colonel Downing
    (fd 'hood 'melissa)    ; ...is named after Sir Barnacle's daughter
    (let ([gabrielles-father (car (assoc-tail 'gabrielle f->d))] ; Gabrielle's father owns the yacht that is named after Dr. Parker's daughter
          [parkers-daughter (cdr (assoc 'parker f->d))])
      (fy gabrielles-father parkers-daughter))
    (let ([fd0 (car f->d)] ; Each of the five also has one daughter and each has named his yacht after a daughter of one of the others.
          [fd1 (car (cdr f->d))]
          [fd2 (car (cdr (cdr f->d)))]
          [fd3 (car (cdr (cdr (cdr f->d))))]
          [fd4 (car (cdr (cdr (cdr (cdr f->d)))))])
      (require (not (member fd0 f->y)))
      (require (not (member fd1 f->y)))
      (require (not (member fd2 f->y)))
      (require (not (member fd3 f->y)))
      (require (not (member fd4 f->y))))
    (list 'fd f->d 'fy f->y)))
  '((fd ((downing . lorna) ; father->daughter mapping
         (hall . gabrielle)
         (hood . melissa)
         (moore . mary-ann)
         (parker . rosalind))
     fy ((downing . melissa) ; father->yacht mapping
         (hall . rosalind)
         (hood . gabrielle)
         (moore . lorna)
         (parker . mary-ann)))))

;;; copy except we don't know mary-ann's father
(define-test (length (eval-all
  '(let* ([fathers '(downing hall hood moore parker)]
          [daughters '(gabrielle lorna mary-ann melissa rosalind)]
          [f->d (map2 cons fathers (permutations daughters))]
          [f->y (map2 cons fathers (permutations daughters))])
    (define (fd father daughter) (require (member (cons father daughter) f->d)))
    (define (fy father yacht) (require (member (cons father yacht) f->y)))
    ;;; requirements
    ; (fd 'moore 'mary-ann)  ; Mary Ann Moore
    (fy 'hood 'gabrielle)  ; Sir Barnacle's yacht is the Gabrielle,
    (fy 'moore 'lorna)     ; Mr. Moore owns the Lorna;
    (fy 'hall 'rosalind)   ; Mr. Hall the Rosalind.
    (fy 'downing 'melissa) ; The Melissa, owned by Colonel Downing
    (fd 'hood 'melissa)    ; ...is named after Sir Barnacle's daughter
    (let ([gabrielles-father (car (assoc-tail 'gabrielle f->d))] ; Gabrielle's father owns the yacht that is named after Dr. Parker's daughter
          [parkers-daughter (cdr (assoc 'parker f->d))])
      (fy gabrielles-father parkers-daughter))
    (let ([fd0 (car f->d)] ; Each of the five also has one daughter and each has named his yacht after a daughter of one of the others.
          [fd1 (car (cdr f->d))]
          [fd2 (car (cdr (cdr f->d)))]
          [fd3 (car (cdr (cdr (cdr f->d))))]
          [fd4 (car (cdr (cdr (cdr (cdr f->d)))))])
      (require (not (member fd0 f->y)))
      (require (not (member fd1 f->y)))
      (require (not (member fd2 f->y)))
      (require (not (member fd3 f->y)))
      (require (not (member fd4 f->y))))
    (list 'fd f->d 'fy f->y))))
  2)
