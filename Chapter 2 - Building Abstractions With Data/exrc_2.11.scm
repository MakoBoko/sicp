#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound interval) 
  (cdr interval))

(define (lower-bound interval) 
  (car interval))

(define (interval-sign interval) 
  (cond 
    ((> (lower-bound interval) 0)  1)
    ((< (upper-bound interval) 0) -1)
    (else 0)))

;; x+,y+ => [x1y1, x2y2]
;; x+,y- => [x2y1, x1y2] 
;; x+,y0 => [x2y1, x2y2] 
;; x-,y+ => [x1y2, x2y1] 
;; x-,y- => [x2x2, x1y1] 
;; x-,y0 => [x1y2, x1y1] 
;; x0,y+ => [x1y2, x2y2] 
;; x0,y- => [x2y1, x1y1]  
;; x0,y0 => [min(x2x1,x1y2), max(x1x1,x2x2)]  

(define (mul-interval x y) 
  (let ((x1 (lower-bound x)) 
        (x2 (upper-bound x)) 
        (y1 (lower-bound y)) 
        (y2 (upper-bound y))
        (sx (interval-sign x))
        (sy (interval-sign y))
        ) 
    (cond 
      ((and (> sx 0) (> sy 0)) (make-interval (* x1 y1) (* x2 y2)))
      ((and (> sx 0) (< sy 0)) (make-interval (* x2 y1) (* x1 y2)))
      ((and (> sx 0) (= sy 0)) (make-interval (* x2 y1) (* x2 y2)))
      ((and (< sx 0) (> sy 0)) (make-interval (* x1 y2) (* x2 y1)))
      ((and (< sx 0) (< sy 0)) (make-interval (* x2 y2) (* x1 y1)))
      ((and (< sx 0) (= sy 0)) (make-interval (* x1 y2) (* x1 y1)))
      ((and (= sx 0) (> sy 0)) (make-interval (* x1 y2) (* x2 y2)))
      ((and (= sx 0) (< sy 0)) (make-interval (* x2 y1) (* x1 y1)))
      ((and (= sx 0) (= sy 0)) (make-interval (min (* x2 y1) (* x1 x2) ) (max (* x1 y1) (* x2 y2)))))))
