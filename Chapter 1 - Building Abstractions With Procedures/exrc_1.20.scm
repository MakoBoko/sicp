#lang sicp

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 206 40)

;; Normal Order - 18 steps
;; (gcd 206 40) ;; 1 step
;; (gcd 40 (remainder 206 40)) -> (40, 6) ; 1 step + 1 for if  
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40))) -> (6, 4) ;; 1 step + 2 step for if  
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) -> (4, 2) ;; 1 step + 3 steps for if
;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) -> (2, 0) b evaluates to 0 ;; 1 step + 4 steps for it
;; (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ;; 1 step
;; (remainder 6 (remainder 40 6)) ;; 1 step
;; (remainder 6 4) ;; 1 step
;; 2 

;; Applicative Order - 10 stesp
;; (gcd 206 40)
;; (gcd 40 6)
;; (gcd 6 4)
;; (gcd 4 2)
;; (gcd 2 0)
;; 2
