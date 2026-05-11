#lang sicp   ;;applicative order - infinite recurrsion
;;#lang lazy ;;normal order - prints 0

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p))
