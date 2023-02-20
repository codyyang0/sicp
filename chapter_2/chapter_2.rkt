#lang racket
; Exercise 2.27
(define (deep-reverse tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) tree)
    (else
     (append
      (deep-reverse (cdr tree))
      (list (deep-reverse (car tree)))))))