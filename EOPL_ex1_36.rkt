#lang racket

(define number_elements
  (lambda (lst)
    (if (null? lst) '()
        [g (list 0 (car lst)) (number_elements (cdr lst))])))

(define g
  (lambda (first_list last_list)
    (cons first_list (add_index last_list))))

(define add_index
  (lambda (lst)
  (if (null? lst) '()
      (cons (cons (+ 1 (caar lst)) (cdar lst)) (add_index (cdr lst)))
      )))

(number_elements '(1 s 5w t d))