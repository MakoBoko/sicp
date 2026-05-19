#lang sicp

(define (dec x) (- x 1))

(define (pascal-triangle row column) 
  (cond
    ((> column row) 0)
    ((or (= column row) (= column 1)) 1)
    (else (+ (pascal-triangle (dec row) (dec column) ) (pascal-triangle (dec row) column)))))

(pascal-triangle 1 5) ;; not a valid pascal triangle value
(pascal-triangle 5 1) 
(pascal-triangle 5 2) 
(pascal-triangle 5 3) 
(pascal-triangle 5 4) 
(pascal-triangle 5 5) 
