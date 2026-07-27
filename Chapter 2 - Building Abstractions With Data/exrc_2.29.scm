#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; b.
(define (total-weight mobile)
  (define (total-weight-branch b-struc)
    (if (pair? b-struc)
        (total-weight b-struc)
        b-struc))
  (+
    (total-weight-branch (branch-structure (left-branch mobile)))
    (total-weight-branch (branch-structure (right-branch mobile)))))

;; c.
(define (balanced? mobile)
  (let (
        (left-b (left-branch mobile))
        (right-b (right-branch mobile))
        (left-b-s (branch-structure (left-branch mobile)))
        (right-b-s (branch-structure (right-branch mobile))))
    (and (=
           (* (branch-length left-b) (if (pair? left-b-s) (total-weight left-b-s) left-b-s))
           (* (branch-length right-b) (if (pair? right-b-s) (total-weight right-b-s) right-b-s)))
         (if (pair? left-b-s) (balanced? left-b-s) #t)
         (if (pair? right-b-s) (balanced? right-b-s) #t))))

;; d.
;;(define (make-mobile left right)
;;  (cons left right))
;;
;;(define (make-structure length structure)
;;  (cons length structure))
;;
;; Changing the represenation of mobiles would only require the selectors to change, no need to reimplement balanced? or total-weight
;; further we could abstract the check for mobile?, the current pair? will work with both list and cons here so no need to change

;; test cases
;; total = 6
;; balanced = true
(define m1
  (make-mobile
    (make-branch 2 3)
    (make-branch 2 3)))

;; total = 9
;; balanced = true
(define m2
  (make-mobile
    (make-branch 1 6)
    (make-branch 2 3)))

;; total = 10
;; balanced = false
(define m3
  (make-mobile
    (make-branch 1 5)
    (make-branch 2 5)))

;; total = 15
;; balanced = true
(define m4
  (make-mobile
    (make-branch 3 m1)
    (make-branch 2 9)))

;; total = 12
;; balanced = false
(define bad-inner
  (make-mobile
    (make-branch 1 3)
    (make-branch 2 3)))

(define m5
  (make-mobile
    (make-branch 2 bad-inner)
    (make-branch 2 6)))

(total-weight m1)
(total-weight m2)
(total-weight m3)
(total-weight m4)
(total-weight m5)

(balanced? m1)
(balanced? m2)
(balanced? m3)
(balanced? m4)
(balanced? m5)


