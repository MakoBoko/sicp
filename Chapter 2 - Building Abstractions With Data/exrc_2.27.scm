#lang sicp

(define x (list (list 1 2) (list 3 4) (list 5 6)))

(define (reverse l)
  (define (reverse-iter l r)
    (if (null? l)
        r
        (reverse-iter (cdr l) (cons (car l) r))))
  (reverse-iter l nil))

(define (deep-reverse l)
  (define (reverse-iter l r)
    (cond ((null? l) r)
          ((pair? (car l)) (reverse-iter (cdr l) (cons (deep-reverse (car l)) r)))
          (else (reverse-iter (cdr l) (cons (car l) r)))))
  (reverse-iter l nil))

x
(reverse x)
(deep-reverse x)

