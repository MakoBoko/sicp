#lang sicp

;; Ackermann's function - a slight variant IIUC
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) 
		 (A x (- y 1))))))

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; ..... 
;; not going into it deeper but the pattern is understandable
;; ackermann's function explodes in recursion especially for a high x value
(A 1 10) ;;1024
(A 2 4)  ;;65536
(A 3 3)  ;;65536  

(define (f n) (A 0 n))    ;; f(n) = 2n
(define (g n) (A 1 n))	  ;; g(n) = 2^n, 
(define (h n) (A 2 n))    ;; h(n) = 2^^n, power tower 2^2^2^2...
(define (k n) (* 5 n n))  ;; k(n) = 5n^2
