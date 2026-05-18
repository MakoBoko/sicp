#lang sicp

(define (func-rec n) 
  (if (< n 3)
    n
    (+ (func-rec (- n 1)) (* (func-rec (- n 2)) 2) (* (func-rec (- n 3)) 3))
   ))

(func-rec 2)
(func-rec 3)
(func-rec 4)
(func-rec 5)
(func-rec 10)

(define (func-iter n)
	(define (func a b c cnt) 
	  (cond 
	    	((= cnt n) a)
		(else (func b c (+ c (* 2 b) (* 3 a)) (+ cnt 1)))
	    ))
	(if (< n 3)
	  n
	(func 0 1 2 0))
  )
 

(func-iter 2)
(func-iter 3)
(func-iter 4)
(func-iter 5)
(func-iter 10)
