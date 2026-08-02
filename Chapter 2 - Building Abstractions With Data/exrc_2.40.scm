#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; given integer n, generate sequences of pair (i, j) so that 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map
               (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 4)

; (define (prime-sum-pairs n)
;   (map make-pair-sum (filter prime-sum? (unique-pairs n)))
;   )
