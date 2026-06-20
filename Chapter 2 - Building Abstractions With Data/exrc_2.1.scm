#lang sicp

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;; -1/-2 = 1/2; 1/-2 = -1/2; -1/-2 = -1/-2
(define (normalize-sign a b) 
  (* a (/ b (abs b))))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))) 
    (cons (normalize-sign (/ n g) d) (/ (abs d) g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) 
            (* (denom x) (denom y))))

(define (sub-rat x y) 
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) 
            (* (denom x) (denom y))))

(define (mul-rat x y) 
  (make-rat (* (numer x) (numer y)) 
            (* (denom x) (denom y))))

(define (div-rat x y) 
  (make-rat (* (numer x) (denom y)) 
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y)) 
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat -1 -2))
(print-rat one-half)
(define one-third (make-rat -1 3))
(print-rat one-third)
(print-rat (add-rat one-third one-half))
(define one-quarter (make-rat 1 -4))
(print-rat one-quarter)
