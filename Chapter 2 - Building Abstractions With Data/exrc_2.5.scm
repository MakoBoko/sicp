#lang sicp

(define (cons a b) 
  (* (expt 2 a) (expt 3 b)))

(define (decode-pair encoded base) 
  (define (iter encoded base result)
    (if (= (remainder encoded base) 0) 
      (iter (/ encoded base) base (+ result 1)) 
      result))
  (iter encoded base 0))

(define (car pair) 
  (decode-pair pair 2))

(define (cdr pair) 
  (decode-pair pair 3))

(define pair (cons 5 10))
(car pair)
(cdr pair)
