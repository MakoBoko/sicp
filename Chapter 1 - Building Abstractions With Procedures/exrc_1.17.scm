#lang sicp

(define (double x) (+ x x)) 
(define (halve x) (/ x 2)) 
(define (even? a) (= (remainder a 2) 0))

;; even => a*b = (a*2)*(b/2)
;; odd => a*b = a*(b-1) + a
(define (* a b) 
  (cond 
    ((= b 0) 0)
    ((even? b) (* (double a) (halve b)))
    (else (+ (* a (- b 1)) a))))

(* 1 0)
(* 1 1)
(* 3 4)
(* 4 3)
(* 5 5)
(* 8 8)
(* 13 13)
(* 16 16)
(* 10 11)
(* 11 10)
(* 56 19)
(* 133 127)
