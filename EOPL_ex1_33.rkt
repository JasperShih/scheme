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


(define add_leaf
  (lambda (bintree)
    (cond [(is_leaf bintree) (+ 1 bintree)]
          [else (list (contents_of bintree) (add_leaf(lson bintree)) (add_leaf(rson bintree)))]
          )))

;(define add_leaf
 ; (lambda (bintree)
  ;  (cond [(is_leaf bintree) (+ 1 bintree)]
   ;       [])))

(define foo
  (lambda (bintree)
    (cond [(is_leaf bintree) 0]
          [(eqv? (contents_of bintree) 'red) (add_leaf (list (contents_of bintree)
                                                             (foo(lson bintree))
                                                             (foo(rson bintree))))]
          [else (list (contents_of bintree)
                      (foo(lson bintree))
                      (foo(rson bintree)))]
          )))

(foo '(red
       (bar 7 1)
       (red 5 (quux 0 2))))

;(add_leaf '(red 3 (quux 0 1)))










