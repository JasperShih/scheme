#lang racket
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (exchange(car lst))
         (invert (cdr lst))))))

(define exchange
  (lambda (lst)
    (cons (cadr lst)
          (cons (car lst)
                '()))))


(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '())
;(exchange '(1 a))