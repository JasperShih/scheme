#lang racket
(define filter_in
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst)) (cons(car lst) (filter_in pred (cdr lst))));;If we want this element use cons
          (else (filter_in pred (cdr lst))))));;If we don't want the element, just pass (cdr lst)


(filter_in number? '(a 2 (1 3) b 7))