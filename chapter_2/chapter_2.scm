#lang scheme
;Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (>= (* n d) 0)
	(cons (/ n g) (/ d g))
	(cons (- (abs (/ n g))) (abs (/ d g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

;Exercise 2.2
;Segment representation
(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;Point representation
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (same-point p1 p2)
  (and (equal? (x-point p1) (x-point p2))
       (equal? (y-point p1) (y-point p2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (let ((start (start-segment s))
	(end (end-segment s)))
    (let ((s-x (x-point start))
	  (s-y (y-point start))
	  (e-x (x-point end))
	  (e-y (y-point end)))
      (make-point (/ (+ s-x e-x) 2)
		  (/ (+ s-y e-y) 2)))))

;Exercise 2.3 rectangle representation
;rectangle construct by two segment
(define (make-rectangle v-seg h-seg)
  ;check
  (if ())
					;check
  (if (and (< (x-point o) (x-point r-b))
	   (< (y-point r-b) (y-point r-t)))
      (list o r-b r-t)))
