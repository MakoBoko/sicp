#lang sicp

(define (dec x) (- x 1))
(define (inc x) (+ x 1))
(define (square x) (* x x))

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

(define (tan-cf x k) 
  (cont-frac (lambda (i)
               (if (= i 1)
                 x
                 (- (square x))))
             (lambda (i) 
               (- (* 2 i) 1))
             k)) 

(define (tan-cf-iter x k) 
  (cont-frac-iter (lambda (i)
                    (if (= i 1)
                      x
                      (- (square x))))
                  (lambda (i) 
                    (- (* 2 i) 1))
                  k)) 

(tan-cf 10.0 20)
(tan-cf-iter 10.0 20)
