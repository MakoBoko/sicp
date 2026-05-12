#lang sicp

(define (square x)
  (* x x))

(define (abs x) 
  (if (> x 0) x (- x)))

(define (good-enough? x0 x1)
  (< (abs (- x0 x1)) (/ x0 10000)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cubrt-iter guess x)
  (if (good-enough? guess (improve guess x))
       guess 
       (cubrt-iter (improve guess x)
		  x)))

(define (cubrt x) 
  (cubrt-iter 1.0 x))

(cubrt 8)
(cubrt 27)
(cubrt 10648)
