#lang sicp

(define (new-if predicate then-clause else-clause) ;; new-if will loop forever in sqrt-iter because in applicative order it would have to eval the else-clause meaning the sqrt-iter gets stuck in recurrsion
  (cond (predicate then-clause)
	(else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (square x)
  (* x x))

(define (abs x) 
  (if (> x 0) x (- x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
       guess 
       (sqrt-iter (improve guess x)
		  x)))

(define (sqrt x) 
  (sqrt-iter 1.0 x))

(sqrt 4)
