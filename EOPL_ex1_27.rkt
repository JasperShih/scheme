#lang racket

(define flatten
  (lambda (slist)
    (cond ((null? slist) '())
          ((symbol? (car slist))
           (cons (car slist) (flatten (cdr slist))))
          ;(car slist) is a slist
          (else (append(flatten (car slist))
                       (flatten (cdr slist))
                )
          ))))

(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))
(flatten '(a b (() (c))))