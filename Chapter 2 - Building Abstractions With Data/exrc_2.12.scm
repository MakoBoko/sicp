#lang sicp

(define (make-center-percent center percent) 
  (let ((center-percent (* center (/ percent 100))))
    (cons (- center center-percent) (+ center center-percent))))

(define (upper-bound interval) 
  (cdr interval))

(define (lower-bound interval) 
  (car interval))

(define (center interval) 
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

(define (percent interval) 
  (let ((interval-center (center interval)))
    (/ (* 100 (- (upper-bound interval) interval-center)) interval-center)))


(define interval1 (make-center-percent 100 40))
(define interval2 (make-center-percent 262 20))

(percent interval1)
(percent interval2)
