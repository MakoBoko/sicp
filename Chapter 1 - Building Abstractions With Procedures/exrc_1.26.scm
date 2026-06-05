#lang sicp

;; Louis Reasoner version of expmod, where square is replaced with explicit multiplication
;; in the book Eva Lu Ator says that this turns expmod from O(logn) -> O(n)
;; the reason is that it calls expmod twice, esentially doing the same computation twice instead of once
;; changing it from linear-recursive into tree-recursive process
(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
      (remainder (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) m))
    (else 
      (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
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
