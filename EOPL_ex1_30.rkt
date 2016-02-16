#lang racket

(define sort/predicate
  (lambda (pred loi)
    (cond [(eqv? pred <) (sort pred loi)]
          [(eqv? pred >) (sort pred loi)])))


(define sort
  (lambda (pred lst)
    (cond [(null? lst) '()]
          [else (insert pred (car lst) (sort pred (cdr lst)))]
          )))


(define insert
  (lambda (pred sym lst)
    (cond [(null? lst) (list sym)]
          [(or(pred sym [car lst]) (= sym [car lst])) (cons sym lst)]
          [else (cons (car lst) (insert pred sym [cdr lst]))]
          )))

(sort/predicate > '(8 2 5 2 3))