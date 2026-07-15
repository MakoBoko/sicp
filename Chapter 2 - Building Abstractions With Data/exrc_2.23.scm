#lang sicp

(define (for-each f items)
  (if (null? items)
      #t
      (begin
        (f (car items))
        (for-each f (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
