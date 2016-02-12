#lang racket

(define up
  (lambda (lst)
    (cond ((null? lst) '())
          ((symbol? (car lst))
           (append(cons (car lst) '())
                  (up (cdr lst))))
          (else (append(car lst) (up (cdr lst))))
      )))




(up '((1 2) (3 4)))
(up '((x (y)) z))
(up '())
(up '(a b (c)))