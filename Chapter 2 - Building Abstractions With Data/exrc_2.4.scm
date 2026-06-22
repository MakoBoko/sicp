#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define set1 (cons 1 2))
(car set1)
(cdr set1)

;; substitution model
;; (car set1)
;; (car (cons 1 2))
;; (car (lambda (m) (m 1 2)))
;; ((lambda (m) (m 1 2)) (lambda (p q) p))
;; ((lambda (p q) p) 1 2)
;; lambda gets called with 1 and 2 returns p which is 1
