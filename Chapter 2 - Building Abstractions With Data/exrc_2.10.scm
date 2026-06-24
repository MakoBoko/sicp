#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound interval) 
  (cdr interval))

(define (lower-bound interval) 
  (car interval))

(define (mul-interval x y) 
  (let ((p1 (* (lower-bound x) (lower-bound y))) 
        (p2 (* (lower-bound x) (upper-bound y))) 
        (p3 (* (upper-bound x) (lower-bound y))) 
        (p4 (* (upper-bound x) (upper-bound y)))) 
    (make-interval 
      (min p1 p2 p3 p4) 
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((y1 (lower-bound y)) 
        (y2 (upper-bound y))) 
  (if (and (<= y1 0) (>= y2 0))
    (error "Cannot divide by an interval that spans 0.")
    (mul-interval x (make-interval (/ 1.0 y2) (/ 1.0 y1))))))

(div-interval (make-interval 1 1) (make-interval -2 2)) 
