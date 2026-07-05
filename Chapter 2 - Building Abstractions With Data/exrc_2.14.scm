#lang sicp

(define (make-interval a b) (cons a b))

(define (make-center-percent center percent) 
  (let ((center-percent (* center (/ percent 100))))
    (cons (- center center-percent) (+ center center-percent))))

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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) 
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (center interval) 
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

(define (percent interval) 
  (let ((interval-center (center interval)))
    (/ (* 100 (- (upper-bound interval) interval-center)) interval-center)))

(define (print-percent-interval interval) 
  (newline)
  (display (center interval))
  (display " (+-) ")
  (display (percent interval))
  (display "%"))

(define a (make-center-percent 40.0 1))
(define b (make-center-percent 50.0 2))

(print-percent-interval (par1 a a))
(print-percent-interval (par2 a a))

(print-percent-interval (par1 a b))
(print-percent-interval (par2 a b))

;; interval arithmetic dependency problem  
(print-percent-interval (div-interval a a))
(print-percent-interval (div-interval a b))

(print-percent-interval (mul-interval a a))
(print-percent-interval (mul-interval a b))



