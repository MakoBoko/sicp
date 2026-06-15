#lang sicp

(define (dec x) (- x 1))
(define (inc x) (+ x 1))

(define (cont-frac n d k)
  (define (cont-frac-helper i)
    (let (
          (ni (n i))
          (di (d i)))
   (if (= i k) 
     (/ ni di)
     (/ ni (+ di (cont-frac-helper (inc i)))))))
  (cont-frac-helper 1))

(define (cont-frac-iter n d k) 
  (define (cont-frac-helper result k )
    (let (
          (nk (n k))
          (dk (d k))) 
      (if (= k 0)
        result
        (cont-frac-helper (/ nk (+ dk result)) (dec k)))))
  (cont-frac-helper 0 k))

(define (euler k-steps) 
  (cont-frac (lambda (i) 1.0) 
             (lambda (i) 
               (if (= (remainder (inc i) 3) 0)
                 (* 2 (/ (inc i) 3))
                 1))
             k-steps)) 

(define (euler-iter k-steps) 
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i) 
                    (if (= (remainder (inc i) 3) 0)
                        (* 2 (/ (inc i) 3))
                        1))
                  k-steps)) 

;; calculation for e - 2 = 2.71828 - 2 = 0.71828
(euler 1)
(euler 2)
(euler 3)
(euler 4)
(euler 5)
(euler 6)
(euler 7)
(euler 8)
(euler 9)
(euler 10)

(euler-iter 10)
