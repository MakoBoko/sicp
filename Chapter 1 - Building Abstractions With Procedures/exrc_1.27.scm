#lang sicp

(define (square x) 
  (* x x))

(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
      (remainder (square (expmod base (/ exp 2) m)) m))
    (else 
      (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test? n a)
  (= (expmod a n n) a))

;; test for every a < n
(define (full-prime-test? n)
  (define (prime-test n a) 
    (cond 
      ((= a 1) true)
      ((fermat-test? n a) (prime-test n (- a 1)))
      (else false)))
  (prime-test n (- n 1)))

(define (test-prime n)
  (cond 
    ((full-prime-test? n) (display n) (display " is a prime number") (newline))
    (else (display n) (display " is NOT a prime number") (newline))))

(display "Actual prime numbers")
(newline)
(test-prime 2)
(test-prime 5)
(test-prime 1009)
(newline)

(display "Non prime numbers")
(newline)
(test-prime 44)
(test-prime 50)
(test-prime 100)
(newline)

(display "Carmichael numbers - False positives for the Fermat test")
(newline)
(test-prime 561)
(test-prime 1105)
(test-prime 1729)
(test-prime 2465)
(test-prime 2821)
(test-prime 6601)
