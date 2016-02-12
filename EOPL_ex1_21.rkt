#lang racket

(define product
 (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (append (one_cross_set(car sos1) sos2)
                (product(cdr sos1) sos2)))))

(define one_cross_set
  (lambda (one set)
      (map (lambda (element) (list one element))set)))

;(one_cross_set 'a '(1 2 3))
(product '(a b) '(1 2 3))













