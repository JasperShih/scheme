#lang racket
(define down
  (lambda (lst)
    (map (lambda (element)
           (list element))
         lst)))

(define down_2
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list(car lst)) (down (cdr lst))))))

(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (morw (complicated)) object))