#lang sicp

(define (square x) 
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
      (check-remainder-square (expmod base (/ exp 2) m) m))
    (else 
      (remainder (* base (expmod base (- exp 1) m)) m))))

(define (check-remainder-square n m)
    ;;if (n > 1 && n < (m-1) && (n^2)%m==1)
    ;;  return 0;
    ;;else 
    ;; return (n^2)%m;
    (if 
      (and (and (> n 1) (< n (- m 1))) (= (remainder (square n) m) 1))
      0 
      (remainder (square n) m)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (test-prime n)
  (cond 
    ((fast-prime? n 10) (display n) (display " is a prime number") (newline))
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

(display "Carmichael numbers - Should not fool the Miller-Rabin test")
(newline)
(test-prime 561)
(test-prime 1105)
(test-prime 1729)
(test-prime 2465)
(test-prime 2821)
(test-prime 6601)
