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

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

;; Alyssa P. Hacker version of expmod
;; This version explodes the number, in this version first we calculate the fast-expt then we apply the remainder
;; lets say we test primality for m=1009 with base=400, we would first have to calculate 400^1009 then apply remainder 1009 on it
;; we can see that 400^1009 is a realy HUGE number,
;; the original expmod keeps the values between [0, m-1] because we are reducing each square with remainder 1009
(define (expmod-v2 base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod-v2 a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (even? n)
  (= (remainder n 2) 0))

(define (search-for-primes p n)
  (cond 
    ((= n 0) (newline) (display "FINISHED"))
    ((even? p) (search-for-primes (+ p 1) n))
    ((fast-prime? p 10) (timed-prime-test p) (search-for-primes (+ p 2) (- n 1)))
    (else (search-for-primes (+ p 2) n ))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
