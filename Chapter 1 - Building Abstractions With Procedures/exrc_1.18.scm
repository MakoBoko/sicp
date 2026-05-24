#lang sicp

(define (double x) (+ x x)) 
(define (halve x) (/ x 2)) 

(define (even? a) (= (remainder a 2) 0))

;; at start y = 0
;; even => x*z + y = (2*x)*(z/2) + y         ;; x = 2*x ;; y = y   ;; z = z/2
;; odd 	=> x*z + y = (2*x)*((z-1)/2) + (y+x) ;; x = 2*x ;; y = y+x ;; z = (z-1)/2

(define (* a b) 
  (define (iter-mult x y z)
    (cond ((= z 0) y)
	  ((even? z) (iter-mult (double x) y (halve z)))
	  (else (iter-mult (double x) (+ x y) (halve (- z 1))))
	  ))
   (iter-mult a 0 b))

(* 1 0)
(* 1 1)
(* 3 4)
(* 4 3)
(* 5 5)
(* 8 8)
(* 13 13)
(* 16 16)
(* 17 19)
(* 10 11)
(* 11 10)
(* 56 19)
(* 133 127)
