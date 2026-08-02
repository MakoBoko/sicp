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

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
          (filter predicate (cdr sequence)))))

(define (triples-i t)
  (car t))

(define (triples-j t)
  (car (cdr t)))

(define (triples-k t)
  (car (cdr (cdr t))))

;;thought would be good to create a procedore (ordered-distinct n d) - n - to what number , d - dimension of the ordered set

; (define (ordered-distinct n d) ())

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (ordered-distinct-triples n)
  (flatmap
    (lambda (i)
      (flatmap
        (lambda (j) (map
                      (lambda (k) (list i j k))
                      (remove j (remove i (enumerate-interval 1 n)))
                      ))
        (remove i (enumerate-interval 1 n))))
    (enumerate-interval 1 n)))

(define (ordered-distinct-triples-sum n s)
  (filter
    (lambda (t) (=
                  s
                  (+ (triples-i t) (triples-j t) (triples-k t))))
    (ordered-distinct-triples n)))

(ordered-distinct-triples-sum 4 8)
