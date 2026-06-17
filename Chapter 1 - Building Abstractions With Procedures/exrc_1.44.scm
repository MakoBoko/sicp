#lang sicp

(define (dec x) (- x 1))
(define (average a b) (/ (+ a b) 2))
(define dx 0.00001)

(define (noisy x) (+ x (* 0.1 (sin (* 100 x)))))

(define (compose f g) 
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (helper-f result-f n)
    (if (= n 0) 
      (lambda (x) (result-f x))
      (helper-f (compose result-f f) (dec n))))
  (helper-f (lambda (x) x) n))

(define (smooth f) 
  (lambda (x) (average (average (f (- x dx)) (f (+ x dx))) (f x))))

(define (smooth-n f n)
  ((repeated smooth n) f))  

(noisy 1)
(noisy 1.1)
((smooth noisy) 1)
((smooth noisy) 1.1)
((smooth-n noisy 5) 1)
