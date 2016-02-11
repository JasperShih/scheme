#lang racket

(define list_set
  (lambda (lst index value)
    (if (null? lst)
        (error "Index out of range")
        (if (zero? index)
            (cons value (cdr lst))
            (cons (car lst)
                  (list_set (cdr lst) (- index 1) value))))))

;I should use condition statement to replace
;2-levels if statement here.

(list_set '(a b c d) 2 '(1 2))
(list_set '(1) 0 '(1 2))