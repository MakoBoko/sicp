#lang sicp

(define (average x y)
  (/ (+ x y) 2))
(define tolerance 0.00001)

(define (iterative-improve good-enough? improve)
  (define (helper guess)
    (let ((improved-guess (improve guess)))
      (if (good-enough? guess improved-guess) 
        guess 
        (helper improved-guess))))
  helper)

(define (sqrt x) 
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? x0 x1)
    (< (abs (- x0 x1)) (/ x0 10000)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f) 
  (define (good-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve good-enough? f) 1.0))

(sqrt 4)
(sqrt 9)
(sqrt 25)
(sqrt 144)

(fixed-point (lambda (x) (+ 1 (/ 1 x))))
