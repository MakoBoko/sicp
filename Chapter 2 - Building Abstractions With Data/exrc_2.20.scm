#lang sicp

(define (even? n) (= (remainder n 2) 0))

(define (reverse l)
  (define (reverse-iter l r)
    (if (null? l)
        r
        (reverse-iter (cdr l) (cons (car l) r))))
  (reverse-iter l nil))

(define (same-parity x . y)
  (let ((even-num? (even? x)))
    (define (same-parity-iter result nums)
      (cond ((null? nums) (reverse result))
            ((eq? even-num? (even? (car nums)))
             (same-parity-iter (cons (car nums) result) (cdr nums))) ;; same parity add to result
            (else
              (same-parity-iter result (cdr nums))))) ;; different parity continue through the list
    (same-parity-iter (list x) y)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
