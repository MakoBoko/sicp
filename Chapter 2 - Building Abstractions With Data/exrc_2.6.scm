#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n) 
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (inc x) (+ x 1))

(define (addition n m) 
  (lambda (f) 
    (lambda (x) 
      ((m f) ((n f) x)))))

((zero inc) 0)               ;; 0
(((add-1 zero) inc) 0)       ;; 1
((one inc) 0)                ;; 1
((two inc) 0)                ;; 2
(((add-1 two) inc) 0)        ;; 3
(((addition one two) inc) 0) ;; 3
(((addition two two) inc) 0) ;; 4

;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; ((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
;; ((lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
;; ((lambda (f) (lambda (x) (f x)))) -- same as our one definition
