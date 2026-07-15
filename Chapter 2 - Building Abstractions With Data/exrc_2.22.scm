#lang sicp

(define (square x) (* x x))

(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))


(square-list-1 (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))

;; the second square-list that louis reasoner defined does not work properly because of the way that lists are constructed
;; answer is a list and in the second definition it gets put as the value instead of being appended to the end of the list
;; so we would get
;; [[], 1]
;; [[[],1], 4]
;; [[[[],1], 4], 9]
;; [[[[[],1], 4], 9], 16]
