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

(define (calc-golden-ratio k-steps) 
  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k-steps)) 

(define (calc-golden-ratio-iter k-steps) 
  (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) k-steps)) 

(calc-golden-ratio 1)  ;; 1
(calc-golden-ratio 2)  ;; 0.5
(calc-golden-ratio 3)  ;; 0.6667
(calc-golden-ratio 4)  ;; 0.6
(calc-golden-ratio 5)  ;; 0.625
(calc-golden-ratio 6)  ;; 0.6154
(calc-golden-ratio 7)  ;; 0.6190
(calc-golden-ratio 8)  ;; 0.6176
(calc-golden-ratio 9)  ;; 0.6182
(calc-golden-ratio 10) ;; 0.6180 - 10 steps needed to get approximation accurate to 4 decimal places for 1/phi

(calc-golden-ratio-iter 10)
