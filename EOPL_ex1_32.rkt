#lang racket

(define double_tree
  (lambda (bin_tree)
    (cond [(integer? bin_tree) (* bin_tree 2)]
          [else (list (car bin_tree)
                      (double_tree (cadr bin_tree))
                      (double_tree (caddr bin_tree)))]
          )))

(double_tree '1)
(double_tree '(foo 1 2))
(double_tree '(bar 1 (foo 1 2)))
(double_tree '(baz
               (bar 1 (foo 1 2))
               (biz 4 5)))
