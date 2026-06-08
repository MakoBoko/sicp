#lang sicp

(define (product term a next b) 
  (if (> a b) 
    1 
    (* (term a) (product term (next a) next b))
  ))

(define (product-iter term a next b) 
  (define (product a result) 
    (if (> a b) 
      result
      (product (next a) (* result (term a)))))
  (product a 1))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (identity x) x)
(define (square x) (* x x))

(define (factorial n)
  (product-iter identity 1 inc n))

(define (approx-pi steps)   
  (define (wallis-product n) 
    (define (term n)
      (define t (* 4.0 (square n))) ;; t = 4*n^2
      (/ t (dec t)))                ;; 4*n^2 / 4*n^2 - 1 - wallis product
    (product-iter term 1 inc n))
  (* (wallis-product steps) 2))

(product square 1 inc 4)
(product-iter square 1 inc 4)

(product square 1 inc 6)
(product-iter square 1 inc 6)

(factorial 5)
(factorial 7)

(approx-pi 10)
(approx-pi 100)
(approx-pi 1000)
