#lang racket

(define
  (pwr_sets lst)
  (cond
    [(empty? lst) '(())]
    [else (helper (car lst) (pwr_sets (cdr lst)))]))

(define
  (helper elem lst)
  (cond
    [(empty? lst) '()]
    [else (cons (cons elem (car lst))
                (cons (car lst)
                      (helper elem (cdr lst))))]))

(pwr_sets '(1 2 3))
