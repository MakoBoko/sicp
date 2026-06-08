#lang sicp

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx) 
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (even? x) (= (remainder x 2) 0))

(define (integral-simpson f a b n) 
  (define h (/ (- b a) n))

  (define (y-k k) 
    (f (+ a (* k h)))) ;; yk = f(a+kh)

  (define (term k) 
    (cond ((or (= k 0) (= k n)) (y-k k)) ;; for k = 0 or n the constant is 1, for even 2, for odd 4 
          ((even? k) (* 2 (y-k k)))
          (else (* 4 (y-k k)))))

  (* (/ h 3.0) (sum term 0 inc n)))

(integral cube 0 1 0.01)
(integral-simpson cube 0 1 100)
(integral-simpson cube 0 1 1000) 
