#lang racket

;Lazy-Evaluation

(define (p) (p))

(define test
  (lambda (x y)
    (if (= (x) 0)
        0
        (y))))

(test (lambda () 0) (lambda () (p)))

