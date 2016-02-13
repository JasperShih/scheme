#lang racket

(define sort
  (lambda (lst)
    (cond [(null? lst) '()]
          [else (insert (car lst) (sort (cdr lst)))]
          )))


(define insert
  (lambda (sym lst)
    (cond [(null? lst) (list sym)]
          [(<= sym [car lst]) (cons sym lst)]
          [(> sym [car lst]) (cons (car lst) (insert sym [cdr lst]))]
          [else (error 'error)]
          )))

;(insert 5 '(0 2 8 9))
(sort '(8 2 5 2 3))

