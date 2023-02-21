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
(define (same-point? p1 p2)
  (and (equal? (x-point p1) (x-point p2))
       (equal? (y-point p1) (y-point p2))))

(define (square x)
  (* x x))

(define (segment-length seg)
  (let ((start (start-segment seg))
	(end (end-segment seg)))
    (let ((s-x (x-point start))
	  (s-y (y-point start))
	  (e-x (x-point end))
	  (e-y (y-point end)))
      (sqrt (+ (square (- s-y e-y))
	       (square (- s-x e-x)))))))

(define (make-rectangle seg1 seg2)
  (let ((s1-start (start-segment seg1))
	(s1-end (end-segment seg1))
	(s2-start (start-segment seg2))
	(s2-end (end-segment seg2)))
    (let ((s1-length (segment-length seg1))
	  (s2-length (segment-length seg2)))
      (let ((leng (sqrt (+ (square s1-length)
			   (square s2-length)))))
	(define (construct p1 p2 p3 p4)
	  (if (and (same-point? p1 p2)
		   (not (same-point? p3 p4))
		   (= leng (segment-length (make-segment p3 p4))))
	      (cons seg1 seg2)
	      '()))
	(let ((rec1 (construct s1-start s2-start s1-end s2-end))
	      (rec2 (construct s1-start s2-end s1-end s2-start))
	      (rec3 (construct s1-end s2-start s1-start s2-end))
	      (rec4 (construct s1-end s2-end s1-start s2-start)))
	  (cond ((not (null? rec1)) rec1)
		((not (null? rec2)) rec2)
		((not (null? rec3)) rec3)
		((not (null? rec4)) rec4)
		(else "ERROR construct rectangle: " (list seg1 seg2))))))))

(define (seg1-rectangle rect) (car rect))
(define (seg2-rectangle rect) (cdr rect))

(define (perimeter rect)
  (let ((seg1 (seg1-rectangle rect))
	(seg2 (seg2-rectangle rect)))
    (let ((leng1 (segment-length seg1))
	  (leng2 (segment-length seg2)))
      (* 2 (+ leng1 leng2)))))

(define (area rect)
  (let ((seg1 (seg1-rectangle rect))
	(seg2 (seg2-rectangle rect)))
    (let ((leng1 (segment-length seg1))
	  (leng2 (segment-length seg2)))
      (* leng1 leng2))))


;Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (x y) x)))

(define (cdr z)
  (z (lambda (x y) y)))


; Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

; Exercise 2.8
(define (sub-interval x y)
  (let ((p1 (max (lower-bound x) (lower-bound y)))
	(p2 (min (upper-bound x) (upper-bound y))))
    (make-interval p1 p2)))

(define (mul-interval x y)
  (let ((p1 (* lower-bound x) (lower-bound y))
	(p2 (* lower-bound x) (upper-bound y))
	(p3 (* upper-bound x) (lower-bound y))
	(p4 (* upper-bound x) (upper-bound y)))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y)))
      (error "Division error (interval spans 0)" y)
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))
; Exercise 2.7
(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (if (> (car x) (cdr x))
      (car x)
      (cdr x)))

(define (lower-bound x)
  (if (> (car x) (cdr x))
      (cdr x)
      (car x)))

;Exercise 2.10
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

; Exercise 2.12
(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (center-2 i)
  (center i))

(define (percent i)
  (/ (width i) (center i)))

;Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;Exercise 2.18
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

;Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;Exercise 2.23
(define (for-each f items)
  (when (not (null? items))
    (begin
      (f (car items))
      (for-each f (cdr items)))))

; A Picture Language
; painter is a procedure representation
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter))))
      (below (flip-vert half) half))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;High order operation
;tl tr bl br are procedures
;square-of-four return a procedure that argument is painter
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (square-list painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; Exercise 2.45
(define (split f1 f2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split f1 f2) painter (- n 1))))
	  (f1 painter (f2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
	       (scale-vect (ycor-vect v) (edge2-frame frame))))))

;Exercise 2.46 vector representation
(define (make-vect x y) (cons x y))
(define (xcor-vector v) (car v))
(define (ycor-vector v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vector v1) (xcor-vector v2))
	     (+ (ycor-vector v1) (ycor-vector v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vector v1) (xcor-vector v2))
	     (- (ycor-vector v1) (ycor-vector v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vector v))
	     (* s (ycor-vector v))))

; Exercise 2.7 frame representation
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

;arguments is same, but represent is different
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

;segments -> painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame)
	 (start-segment segment))
	((frame-coord-map frame)
	 (end-segment segment))))
     segment-list)))

;Exercise 2.48 segment representation
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

;Exercise 2.49 use segments->painter draw
; a draws the outline
(segments->painter (list (make-segment (make-vector 0 0) (make-vector 0 1))
			 (make-segment (make-vector 0 0) (make-vector 1 0))
			 (make-segment (make-vector 1 0) (make-vector 1 1))
			 (make-segment (make-vector 0 1) (make-vector 1 1))))
; b draw an "X"
(segments->painter (list (make-segment (make-vector 0 0) (make-vector 1 1))
			 (make-segment (make-vector 0 1) (make-vector 1 0))))

; draw a "X" in a frame
((segments->painter (list (make-segment (make-vector 0 0) (make-vector 1 1))
			  (make-segment (make-vector 0 1) (make-vector 1 0))))
 (make-frame (make-vect 0.0 0.0)
	     (make-vect 1.0 0.0)
	     (make-vect 0.0 1.0)))

(define x-painter
  (segments->painter (list (make-segment (make-vector 0 0) (make-vector 1 1))
			   (make-segment (make-vector 0 1) (make-vector 1 0)))))

(define init-frame
  (make-frame (make-vect 0.0 0.0)
	      (make-vect 1.0 0.0)
	      (make-vect 0.0 1.0)))
;two "X" beside
((beside x-painter x-painter) init-frame)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter (make-frame
		  new-origin
		  (sub-vect (m corner1) new-origin)
		  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0) ;new origin
		     (make-vect 1.0 1.0) ;new end of edge1
		     (make-vect 0.0 0.0) ;new end of edge2
		     ))

(define (shrink-to-upper-right painter)
  (transform-painter
   painter (make-vect 0.5 0.5) (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (square-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter
	    painter1
	    (make-vect 0.0 0.0)
	    split-point
	    (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter
	    painter2
	    split-point
	    (make-vect 1.0 0.0)
	    (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

; painter is a procedure which argument is frame
; when painter draw lines? I am confused.
