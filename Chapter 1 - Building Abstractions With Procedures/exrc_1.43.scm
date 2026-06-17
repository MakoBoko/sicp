#lang sicp

(define (dec x) (- x 1))
(define (square x) (* x x))

(define (compose f g) 
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (helper-f result-f n)
    (if (= n 0) 
      (lambda (x) (result-f x))
      (helper-f (compose result-f f) (dec n))))
  (helper-f (lambda (x) x) n))

((repeated square 2) 5)
