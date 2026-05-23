#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)  
  (define (expt-iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (expt-iter a (square b) (/ n 2)))
	  (else (expt-iter (* a b) (square b) (/ (- n 1) 2)))))
  (expt-iter 1 b n))


(fast-expt 2 0)
(fast-expt-iter 2 0)

(fast-expt 2 1)
(fast-expt-iter 2 1)

(fast-expt 2 2)
(fast-expt-iter 2 2)

(fast-expt 2 3)
(fast-expt-iter 2 3)

(fast-expt 2 4)
(fast-expt-iter 2 4)

(fast-expt 2 10)
(fast-expt-iter 2 10)

(fast-expt 2 11)
(fast-expt-iter 2 11)
