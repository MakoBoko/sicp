#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b) 
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (accumulate a result) 
    (if (> a b) 
      result
      (accumulate (next a) (combiner (term a) result))))
  (accumulate a null-value))

(define (cube x) (* x x x))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(accumulate-iter + 0 cube 0 inc 3)    ;; (sum cube 0 inc 3)
(accumulate-iter + 0 cube 1 inc 10)   ;; (sum cube 1 inc 10)
(accumulate-iter * 1 square 1 inc 4)  ;; (product square 1 inc 4)
(accumulate-iter * 1 square 1 inc 6)  ;; (product square 1 inc 6)
