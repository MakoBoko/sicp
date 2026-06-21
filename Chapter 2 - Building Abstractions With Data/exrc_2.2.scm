#lang sicp

(define (average a b) (/ (+ a b) 2))

;; point - (x,y) => cons(car,cdr)
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment a b) (cons a b))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

;; a-segment, b-segment; a(x1,y1); b(x2,y2) -> c((x1+x2)/2, (y1+y2)/2)
(define (midpoint-segment segment) 
  (make-point (average (x-point (start-segment segment)) (x-point (end-segment segment))) 
              (average (y-point (start-segment segment)) (y-point (end-segment segment))))) 

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment segment)
  (newline)
  (display "Start-Segment:")
  (print-point (start-segment segment))
  (newline)
  (display "Mid-Segment:")
  (print-point (midpoint-segment segment))
  (newline)
  (display "End-Segment:")
  (print-point (end-segment segment)))


(define segment-1 
  (make-segment 
    (make-point 1 1) 
    (make-point 1 10)))

(print-segment segment-1)


