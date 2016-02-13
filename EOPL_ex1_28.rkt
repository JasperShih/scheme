#lang racket

(define merge
  (lambda (list1 list2)
    (cond ((or(null? list1) (null? list2)) (append list1 list2))
          ((>(car list1)(car list2)) (cons (car list2) (merge list1 (cdr list2))))
          ;(<=(car list1)(car list2))
          (else (cons (car list1) (merge (cdr list1) list2)))
      )))

(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))