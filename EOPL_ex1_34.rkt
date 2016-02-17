#lang racket


(define path
  (lambda (target bst)
    (cond [(eqv? (car bst) target) '()]
          [(< target (car bst)) (cons 'left (path target (cadr bst)))]
          [(> target (car bst)) (cons 'right (path target (caddr bst)))]
          [else (error "error")]
          )))

(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))

(path 12 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))



