#lang sicp

(define tolerance 0.00001)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (inc x) (+ x 1))

(define (average-damp f) 
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess) 
    (let ((next (f guess))) 
      (if (close-enough? guess next) 
        next 
        (try next))))
  (try first-guess))

(define (compose f g) 
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (helper-f result-f n)
    (if (= n 0) 
      (lambda (x) (result-f x))
      (helper-f (compose result-f f) (dec n))))
  (helper-f (lambda (x) x) n))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (fourth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (expt y 3))))) 1.0))

(define (fifth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (expt y 4))))) 1.0))

;; additional damp at 2, 4, 8, 16, 32, 2^n ..... 
(define (average-damp-n f n) 
  (define (damp-steps result n) 
    (if (< n 2) 
      result 
      (damp-steps (inc result) (/ n 2))))
  ((repeated average-damp (damp-steps 0 n)) f))

(define (n-root x n)
  (fixed-point (average-damp-n (lambda (y) (/ x (expt y (- n 1)))) n) 1.0))

(cube-root 27)
(fourth-root 81)
(fifth-root 243)

(n-root 3 1)
(n-root 9 2)
(n-root 27 3)
(n-root 81 4)
(n-root 243 5)
(n-root 729 6)
(n-root 2187 7)
(n-root 6561 8)
(n-root 19683 9)
(n-root 43046721 16)
