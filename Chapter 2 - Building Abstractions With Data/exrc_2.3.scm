#lang sicp

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment a b) (cons a b))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-rectangle segment-a segment-b) 
  (cons segment-a segment-b))

(define (rectangle-a r) 
  (x-point (start-segment r)))
(define (rectangle-b r) 
  (y-point (start-segment r)))
(define (rectangle-c r) 
  (x-point (end-segment r)))
(define (rectangle-d r) 
  (y-point (end-segment r)))

;; a------d
;; |      |
;; |      |
;; b------c
;; lets assume (a-b) and (d-c) are always parallel to the y axis and (a-d) and (b-c) are always parallel to the x axis for easier computation

;;|a.y-b.y|
(define (rectangle-height r)
  (abs (- (y-point (rectangle-a r)) (y-point (rectangle-b r)))) )
;;|d.x-a.x|
(define (rectangle-width r) 
  (abs (- (x-point (rectangle-d r)) (x-point (rectangle-a r)))))

;; perimeter - 2*(height+*width)
(define (rectangle-perimeter r) 
  (* 2 (+ (rectangle-height r) (rectangle-width r))))
;; area - height*width
(define (rectangle-area r) 
  (* (rectangle-height r) (rectangle-width r)))

(define rectangle-1 
  (make-rectangle 
    (make-segment (make-point 0 5) (make-point 0 0))     ;; a,b
    (make-segment (make-point 10 0) (make-point 10 5)))) ;; c,d

(rectangle-perimeter rectangle-1)
(rectangle-area rectangle-1)
