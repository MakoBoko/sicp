#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound interval) 
  (cdr interval))

(define (lower-bound interval) 
  (car interval))

(define (sub-interval x y) 
  (make-interval 
    (- (lower-bound x) (upper-bound y)) 
    (- (upper-bound x) (lower-bound y))))

(define (add-interval x y) 
  (make-interval 
    (+ (lower-bound x) (lower-bound y)) 
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y) 
  (let ((p1 (* (lower-bound x) (lower-bound y))) 
        (p2 (* (lower-bound x) (upper-bound y))) 
        (p3 (* (upper-bound x) (lower-bound y))) 
        (p4 (* (upper-bound x) (upper-bound y)))) 
    (make-interval 
      (min p1 p2 p3 p4) 
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                  (/ 1.0 (upper-bound y)) 
                  (/ 1.0 (lower-bound y)))))

(define (interval-width interval) 
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define inter1 (make-interval 1.0 10.0))
(define inter2 (make-interval 5.0 14.0))

;; for a interval [a,b] - width is (b-a)/2
;; interval sum - [a1,b1] + [a2,b2] = [a1+a2, b1+b2] 


;; sum
;; w([a1+a2, b1+b2]) = (b1+b2-a1-a2)/2
;; w([a1+a2, b1+b2]) = (b1-a1)/2+(b2-a2)/2
;; w([a1+a2, b1+b2]) = w([a1,b1]) + w([a2,b2])

(interval-width (add-interval inter1 inter2))
(+ (interval-width inter1) (interval-width inter2))

;; sub
;; w([a1-b2, b1-a2]) = (b1-a2-a1+b2)/2
;; w([a1-b2, b1-a2]) = (b1-a1)/2 + (b2-a2)/2 
;; w([a1-b2, b1-a2]) = w([a1,b1]) + w([a2,b2])

(interval-width (sub-interval inter1 inter2))
(+ (interval-width inter1) (interval-width inter2))

;; interval mul - [a1,b1]*[a2,b2] = [min{a1b1, a1b2, a2b1, a2b2}, max{a1b1, a1b2, a2b1, a2b2}]
;; let apply width to both sides 
;; let a1,b1,a2,b2 > 1 then [a1,b1]*[a2,b2]=[a1a2,b1b2]
;; width(a1a2,b1b2) = (b1b2-a1a2)/2
;; width(a1a2,b1b2) = (b1b2-a1a2)/2

(interval-width inter1)
(interval-width inter2)
(interval-width (mul-interval inter1 inter2))

(define inter3 (make-interval 20.0 29.0))
(define inter4 (make-interval 30.0 39.0))
(interval-width inter3)
(interval-width inter4)
(interval-width (mul-interval inter3 inter4))

;; intervals with same widths can produce products with different widths
