#lang racket

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



(define number_leaves
  (lambda (BT)
    (car (deal_BT BT 0))))

(define deal_BT
  (lambda (BT counter)
    (cond [(is_BT_type_1? BT) (deal_BT_type_1 counter)]
          ;;BT_type_2
          [else (deal_BT_type_2 BT counter)]
          )))

(define is_BT_type_1?
  (lambda (BT)
    (integer? BT)))

(define deal_BT_type_1
  (lambda (counter)
    (cons counter counter)))

(define deal_BT_type_2
  (lambda (BT_type_2 counter_0)
    (let [(lson_counter_1 (deal_BT (lson BT_type_2) counter_0))]
      (let [(rson_counter_2 (deal_BT (rson BT_type_2) (+ 1 (cdr lson_counter_1))))]
      
      (cons
       (list [car BT_type_2]
             [car lson_counter_1]
             [car rson_counter_2 ])
       (cdr rson_counter_2))

      )
      )))

(number_leaves '(foo (bar 26 (hh (ss 5 8) 8)) (baz 11 (quux 117 14))))






















