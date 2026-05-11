#lang sicp

(define (sqr x) (* x x))

(define (sqrSum x y) (+ (sqr x) (sqr y)))

(define (foo x y z) 
  (cond ((and (< x y) (< x z)) (sqrSum y z)) 
  	((and (< y x) (< y z)) (sqrSum x z)) 
	(else (sqrSum x y))))

(foo 1 2 3)
(foo 3 2 1)
(foo 2 3 1)
