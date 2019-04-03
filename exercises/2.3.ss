#|

Exercise 2.3: Implement a representation for rectangles in a
plane. (Hint: You may want to make use of Exercise 2.2.) In
terms of your constructors and selectors, create procedures
that compute the perimeter and the area of a given
rectangle. Now implement a different representation for
rectangles. Can you design your system with suitable
abstraction barriers, so that the same perimeter and area
procedures will work using either representation?

|#

#| Answer |#

(load-ex "2.2")

; Representation 1: Two perpendicular sides
(define (make-rectangle sa sb)
  (cons sa sb))
(define (a-side r)
  (car r))
(define (b-side r)
  (cdr r))

; Representation 2: Two adjacent corners
#|

(define (a-side r)
  (let ([pa (car r)]
        [pb (cdr r)])
    (make-segment pa (make-point (+ (x-point pa) (- (x-point pb) (x-point pa)))
                                 (y-point pa)))))
(define (b-side r)
  (let ([pa (car r)]
        [pb (cdr r)])
    (make-segment pa (make-point (x-point pa)
                                 (+ (y-point pa) (- (y-point pb) (y-point pa)))))))
|#

; Selectors
(define (distance-segment s)
  (let ([p0 (start-segment s)]
        [p1 (end-segment s)])
  (sqrt (+ (expt (- (x-point p1) (x-point p0)) 2)
           (expt (- (y-point p1) (y-point p0)) 2)))))

(define (rectangle-area r)
  (* (distance-segment (a-side r))
     (distance-segment (b-side r))))

(define (rectangle-perimeter r)
  (* 2 (+ (distance-segment (a-side r)) 
          (distance-segment (b-side r)))))

#| Tests

; r0 with sides {(0,0) to (0,3)} and {(0,0) to (2,0)}
; r1 with sides {(0,0) to (0,3)} and {(0,0) to (2,0)}
; r2 with sides {(0,0) to (0,3)} and {(0,0) to (2,0)}
; r3 with sides {(0,0) to (0,3)} and {(0,0) to (2,0)}
; area=6, perimeter=10

> (define (quick-make a-neg b-neg)
      (let ([a (if a-neg -3 3)]
            [b (if b-neg -2 2)])
    (make-rectangle 
      (make-segment (make-point 0 0) (make-point 0 a))
      (make-segment (make-point 0 0) (make-point b 0)))))
> (define rects (list (quick-make #f #f)
                      (quick-make #f #t)
                      (quick-make #t #f)
                      (quick-make #t #t)))
> (map rectangle-area rects)    
(6 6 6 6)
> (map rectangle-perimeter rects)
(10 10 10 10)                  

; ... paste second representation into console ...

> (define (quick-make a-neg b-neg)
      (let ([a (if a-neg -3 3)]
            [b (if b-neg -2 2)])
    (make-rectangle (make-point 0 0) (make-point a b)))))
> (define rects (list (quick-make #f #f)
                      (quick-make #f #t)
                      (quick-make #t #f)
                      (quick-make #t #t)))
> (map rectangle-area rects)    
(6 6 6 6)
> (map rectangle-perimeter rects)
(10 10 10 10) 

|#