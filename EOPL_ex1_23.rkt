#lang racket

(define list_index
  (lambda (pred lst) (let ((sum (index_sum pred lst)) )
                       (if (eqv? sum (length lst))
                           #f
                           sum))))
(define index_sum
  (lambda (pred lst)
    (cond ((null? lst) 0)
          ((pred (car lst)) 0)
          (else (+ 1 (index_sum pred (cdr lst)))))))

(define length
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ 1 (length(cdr lst)))))))

(list_index number? '(a k (1 3) b 7))
(list_index number? '(a 5 (1 3) b 7))
(list_index symbol? '(a (b c) 17 foo))
(list_index symbol? '(1 2 (a b) 3))