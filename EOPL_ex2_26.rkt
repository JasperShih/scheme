#lang racket

(define count-red
  (lambda (tree counter)
    {case (car tree)
      [(red-node) (list
                    'red-node
                    (count-red (second tree) (+ counter 1))
                    (count-red (third tree) (+ counter 1)))]
      [(blue-node) [define count-tree-lst
                     (lambda (lst)
                       {cond
                         [(null? lst) '()]
                         [else (cons
                                 (count-red (car lst) counter)
                                 (count-tree-lst (cdr lst)))]})]
                   (list 'blue-node (count-tree-lst (cdr tree)))]
      [(leaf-node) (list 'leaf-node counter)]}))

(count-red '(red-node
              (red-node
                (leaf-node 5)
                (blue-node
                  (red-node
                    (leaf-node 5)
                    (leaf-node 100))
                  (leaf-node 20)
                  (leaf-node 6)
                  (leaf-node -87)))
              (leaf-node 0))
           0)
