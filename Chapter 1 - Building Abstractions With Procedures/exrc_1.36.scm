#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (print-guess guess step-count)
    (display step-count)
    (display " --- ")
    (display guess)
    (newline))
  (define (try guess step-count) 
    (let ((next (f guess))) 
      (print-guess guess step-count)
      (if (close-enough? guess next) 
        (begin (print-guess next (+ step-count 1)) next) 
        (try next (+ step-count 1)))))
  (try first-guess 1))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)              ;; without average dampening - 35 steps
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)  ;; with average dampening - 10 steps
