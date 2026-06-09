#lang sicp

(define (square x) 
  (* x x))

(define (inc x)
  (+ x 1))

(define (identity x) x)

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (if (< n 2) #f
    (= n (smallest-divisor n))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond 
    ((> a b) null-value)
    ((filter a) (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
    (else (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (filtered-accumulate-iter filter combiner null-value term a next b)
  (define (accumulate a result) 
    (cond ((> a b) result)
          ((filter a) (accumulate (next a) (combiner (term a) result)))
          (else (accumulate (next a) result))))
  (accumulate a null-value))

;; 1.33.a - sum of squares of the prime numbers in the interval a to b
(define (prime-square-sum a b)
  (filtered-accumulate-iter prime? + 0 square a inc b))

(prime-square-sum 1 10) 
(prime-square-sum 1 100) 

;; 1.33.b - product of all positive integers less than n that are relatively prime to n

(define (relative-primes-product i n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate-iter relative-prime? * 1 identity i inc n))

(relative-primes-product 1 10)
(relative-primes-product 1 50)
