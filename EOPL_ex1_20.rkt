#lang racket
(define count_occurrences
  (lambda (symbol s_list)
    (if (null? s_list)
        0
        (+ ((lambda (element) (if (symbol? element)
                                  (if (eqv? element symbol)
                                      1
                                      0)
                                  (count_occurrences symbol element)))
           (car s_list))
           (count_occurrences symbol (cdr s_list))))))


(count_occurrences 'x '((f x) y (((x z) x))))
(count_occurrences 'x '((f x) y (((x z) () x))))
(count_occurrences 'w '((f x) y (((x z) x))))
(count_occurrences 'x '())