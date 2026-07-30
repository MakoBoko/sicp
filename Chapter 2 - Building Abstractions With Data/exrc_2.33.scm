#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (_ y) (+ y 1)) 0 sequence))

(define (square x) (* x x))

(define list-test (list 1 2 3 4 5))
(define list-test-2 (list 6 7 8 9 10))

(map square list-test)
(append list-test list-test-2)
(length list-test)
