#lang racket 
(define substitute_s_list
  (lambda (new old s_list)
    (if (null? s_list)
        '()
        (map (lambda (s_exp)
               (substitute_s_exp new old s_exp)) s_list))))

(define substitute_s_exp
  (lambda (new old s_exp)
    (if (symbol? s_exp)
        (if (eqv? s_exp old)
            new
            s_exp)
        (substitute_s_list new old s_exp))))

(substitute_s_list 'a 'b '((b c ) (b () d)))