#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; test cases
(define m (list (list 1 2 3) (list 4 5 6)))
(define n (list (list 7 8) (list 9 10) (list 11 12)))
(define v (list 10 20 30))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (map (lambda (y) (dot-product x y))
                cols))
         m)))

(matrix-*-vector m v)
(transpose m)
(transpose n)
(matrix-*-matrix m n)
