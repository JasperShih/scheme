#lang racket
(define exists?
  (lambda (pred lst)
    (cond ((null? lst) #f)
          ((pred (car lst)) #t)
          (else (exists? pred (cdr lst)))
          )))

(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))
