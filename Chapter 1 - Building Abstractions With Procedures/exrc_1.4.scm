#lang sicp

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)

;; If b > 0 evaluates to (+ a b) = a + b
;; If b < 0 evaluates to (- a b) = a - b
