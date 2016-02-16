#lang racket

(define leaf
  (lambda (int)
    int))

(define interior_node
  (lambda (sym left_bi_tree right_bi_tree)
  (list sym left_bi_tree right_bi_tree)))

(define is_leaf
  (lambda (bintree)
    (integer? bintree)))

(define lson
  (lambda (inter_node)
    (cadr inter_node)))

(define rson
  (lambda (inter_node)
    (caddr inter_node)))

(define contents_of
  (lambda (inter_node)
    (cond [(is_leaf inter_node) inter_node]
          [else (car inter_node)]
          )))

(contents_of (interior_node 'bar (leaf 23) (leaf 12)))
(contents_of 15)










