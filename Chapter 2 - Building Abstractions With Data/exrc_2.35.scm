#lang sicp

(define (enumarate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumarate-tree (car tree))
                      (enumarate-tree (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; count-leaves (input tree) => input into accumulate is a list so we have to enumarate the tree
;; tree => enumarate to list => length of list
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumarate-tree t))))


;; test-cases

;; leaves 3
(define t1
  (list 1 2 3))

;; leaves 6
(define t2
  (list
    (list 1 2)
    3
    (list 4 (list 5 6))))

;; leaves 10
(define t3
  (list
    (list 1 (list 2 3))
    (list (list 4) 5)
    (list 6 (list 7 (list 8 9)))
    10))

(count-leaves t1)
(count-leaves t2)
(count-leaves t3)
