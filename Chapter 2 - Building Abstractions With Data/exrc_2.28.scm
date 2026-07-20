#lang sicp

(define x (list (list 1 2) (list 3 4)))

(define (fringe l)
  (define (fringe-iter l r)
    (cond ((null? l) r)
          ((not (pair? l)) (cons l r))
          (else (fringe-iter (car l) (fringe-iter (cdr l) r)))
          ))
  (fringe-iter l nil))

(fringe x)
(fringe (list x x))
